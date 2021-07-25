{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.Contracts.FileStorage.Request
    ( endpoints
    , createFile
    , closeFile
    , checkFile
    , listFiles
    , findFile
    , findFilesWhere
    , findValidFiles
    , FileStorageSchema
    , FileStorageObservable (..)
    , FileStorageEndpoint
    ) where

import           Control.Monad                      hiding (fmap)
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.ByteString.UTF8               as BSU
import qualified Data.Map                           as Map
import           Data.Monoid                        (Last (..))
import           Data.Text                          (Text, pack)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import           Ledger
import qualified Ledger.Constraints                 as Constraints
import qualified Ledger.Typed.Scripts               as Scripts
import           Ledger.Value                       as Value
import           Plutus.Contract                    as Contract hiding (when)
import qualified Plutus.Contracts.Currency          as Currency
import qualified PlutusTx
import           PlutusTx.Prelude                   hiding (Semigroup (..), check)
import           Prelude                            (Semigroup (..), Show (..), String)
import qualified Prelude                            as Haskell
import           Schema                             (ToSchema)
import           Text.Printf                        (printf)

import           Plutus.Contracts.FileStorage.File  (fileTypedValidator, fileValidator, fileValidatorHash)
import           Plutus.Contracts.FileStorage.Types (FileBeginUploadArgs (..), FileDatum (..), FileRedeemer (..),
                                                     FileUploadShardArgs (..), ShardDatum (..))
import           Plutus.Contracts.ScriptToken       (ScriptToken (..), ScriptTokenParams (..), scriptToken,
                                                     scriptTokenPolicy, scriptTokenSymbol)
import           Plutus.Contracts.Utils             (getTypedDatum, txOutHasNFT)

data CreateFileArgs
    = CreateFileArgs
        { crfFileName  :: String
        , crfShardSize :: Integer
        }
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data RenameFileArgs
    = RenameFileArgs
        { rnfSymbol   :: ByteString
        , rnfFileName :: String
        }
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data SetAdminArgs
    = SetAdminArgs
        { saSymbol :: ByteString
        , saAdmin  :: [ByteString]
        }
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data FileIdent
    = FileIdent
        { fidFileName :: String
        , fidSymbol   :: ByteString
        }
    deriving (Show, Generic, FromJSON, ToJSON)

fileTokenName :: TokenName
fileTokenName = "simple-file-storage"

initialFileDatum :: PubKeyHash -> ScriptToken -> ByteString -> Integer -> FileDatum
initialFileDatum pkh scrToken fileName shardSize
    = FileDatum
        { fOwner         = pkh
        , fScriptToken   = scrToken
        , fShardSize     = shardSize
        , fFileName      = fileName
        , fAdmin         = [pkh]
        , fLastUpdatedBy = pkh
        , fUploadLock    = Nothing
        , fShardNFT      = Nothing
        , fChecksum      = Nothing
        }

fileNFT :: FileDatum -> AssetClass
fileNFT = scriptTokenNFT . fScriptToken

createFile :: forall w s. CreateFileArgs -> Contract w s Text FileIdent
createFile CreateFileArgs{..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    utxos <- Contract.utxoAt $ pubKeyHashAddress pkh
    oref <- case Map.toList utxos of
        []             -> throwError "no valid utxo found for the wallet"
        (oref', _) : _ -> pure oref'
    let tokenParams =
            ScriptTokenParams
                { stpTxOutRef   = oref
                , stpScriptHash = fileValidatorHash
                , stpTokenName  = fileTokenName
                }
        scrToken = scriptToken tokenParams
        nft@(AssetClass (cs, _)) = scriptTokenNFT scrToken
        d        = initialFileDatum pkh scrToken bsFileName crfShardSize
        v        = Value.assetClassValue nft 1
        lookups  = Constraints.typedValidatorLookups fileTypedValidator      <>
                   Constraints.otherScript fileValidator                     <>
                   Constraints.mintingPolicy (scriptTokenPolicy tokenParams) <>
                   Constraints.unspentOutputs utxos                          <>
                   Constraints.ownPubKeyHash pkh
        tx       = Constraints.mustPayToTheScript d v     <>
                   Constraints.mustMintValue v            <>
                   Constraints.mustSpendPubKeyOutput oref
    ledgerTx <- Contract.submitTxConstraintsWith lookups tx
    void $ Contract.awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "created a file %s for token %s" (show crfFileName) (show nft)
    pure $ FileIdent crfFileName (unCurrencySymbol cs)
  where
    bsFileName = BSU.fromString crfFileName

closeFile :: forall w s. ByteString -> Contract w s Text FileIdent
closeFile sym = do
    (oref, ot, datum@FileDatum{..}) <- findFile $ CurrencySymbol sym
    -- TODO: close shards as well
    pkh <- pubKeyHash <$> Contract.ownPubKey
    when (pkh /= fOwner) $ throwError "only the owner can close a file"
    let nft@(AssetClass (_, tn)) = fileNFT datum
        tokenParams =
            ScriptTokenParams
                { stpTxOutRef   = scriptTokenTxOutRef fScriptToken
                , stpScriptHash = fileValidatorHash
                , stpTokenName  = tn
                }
    when (scriptToken tokenParams /= fScriptToken) $ throwError "invalid script token, cannot burn nft"
    let fileName = BSU.toString fFileName
        v        = Value.assetClassValue nft 1
        redeemer = Redeemer $ PlutusTx.toData Close
        lookups  = Constraints.typedValidatorLookups fileTypedValidator      <>
                   Constraints.otherScript fileValidator                     <>
                   Constraints.mintingPolicy (scriptTokenPolicy tokenParams) <>
                   Constraints.unspentOutputs (Map.singleton oref ot)        <>
                   Constraints.ownPubKeyHash pkh
        tx       = Constraints.mustSpendScriptOutput oref redeemer <>
                   Constraints.mustMintValue (negate v)
    ledgerTx <- Contract.submitTxConstraintsWith lookups tx
    void $ Contract.awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "closed the file %s for token %s" (show fileName) (show $ fileNFT datum)
    pure $ FileIdent fileName sym

checkFile :: forall w s. ByteString -> Contract w s Text FileDatum
checkFile sym = do
    (_, _, datum) <- findFile $ CurrencySymbol sym
    pure datum

listFiles :: forall w s. Contract w s Text [FileIdent]
listFiles = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    map getFileIdent <$> findFilesWhere (isAdmin pkh)
  where
    isAdmin :: PubKeyHash -> FileDatum -> Bool
    isAdmin pkh FileDatum{..} = pkh `elem` fAdmin

    getFileIdent :: (TxOutRef, TxOutTx, FileDatum) -> FileIdent
    getFileIdent (_,_,datum@FileDatum{..}) = FileIdent (BSU.toString fFileName) sym
      where
        sym = unCurrencySymbol $ fst $ unAssetClass $ fileNFT datum

upload = Haskell.undefined

cancelUpload = Haskell.undefined

remove = Haskell.undefined

renameFile :: forall w s. RenameFileArgs -> Contract w s Text FileIdent
renameFile RenameFileArgs{..} = do
    (oref, ot, datum@FileDatum{..}) <- findFile $ CurrencySymbol rnfSymbol
    pkh <- pubKeyHash <$> Contract.ownPubKey
    when (bsFileName == emptyByteString) $ throwError "filename cannot be empty"
    let oldFileName = BSU.toString fFileName
        d           = datum
                          { fFileName      = bsFileName
                          , fLastUpdatedBy = pkh
                          }
        v           = Value.assetClassValue (fileNFT datum) 1
        redeemer    = Redeemer $ PlutusTx.toData $ Rename bsFileName
        lookups     = Constraints.typedValidatorLookups fileTypedValidator <>
                      Constraints.otherScript fileValidator                <>
                      Constraints.unspentOutputs (Map.singleton oref ot)   <>
                      Constraints.ownPubKeyHash pkh
        tx          = Constraints.mustSpendScriptOutput oref redeemer <>
                      Constraints.mustPayToTheScript d v
    ledgerTx <- Contract.submitTxConstraintsWith lookups tx
    void $ Contract.awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "renamed the file %s to %s for token %s" (show oldFileName) (show rnfFileName) (show $ fileNFT datum)
    pure $ FileIdent rnfFileName rnfSymbol
  where
    bsFileName = BSU.fromString rnfFileName

setFileAdmin :: forall w s. SetAdminArgs -> Contract w s Text FileIdent
setFileAdmin SetAdminArgs{..} = do
    (oref, ot, datum@FileDatum{..}) <- findFile $ CurrencySymbol saSymbol
    pkh <- pubKeyHash <$> Contract.ownPubKey
    when (fOwner `notElem` newAdmin) $ throwError "the owner should be an admin"
    let fileName = BSU.toString fFileName
        d        = datum
                       { fAdmin         = newAdmin
                       , fLastUpdatedBy = pkh
                       }
        v        = Value.assetClassValue (fileNFT datum) 1
        redeemer = Redeemer $ PlutusTx.toData $ SetAdmin newAdmin
        lookups  = Constraints.typedValidatorLookups fileTypedValidator <>
                   Constraints.otherScript fileValidator                <>
                   Constraints.unspentOutputs (Map.singleton oref ot)   <>
                   Constraints.ownPubKeyHash pkh
        tx       = Constraints.mustSpendScriptOutput oref redeemer <>
                   Constraints.mustPayToTheScript d v
    ledgerTx <- Contract.submitTxConstraintsWith lookups tx
    void $ Contract.awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "updated admins for the file %s for token %s" (show fileName) (show $ fileNFT datum)
    pure $ FileIdent fileName saSymbol
  where
    newAdmin = PubKeyHash <$> saAdmin

findValidFiles :: forall w s. Contract w s Text [(TxOutRef, TxOutTx, FileDatum)]
findValidFiles = do
    utxos <- Contract.utxoAt $ scriptHashAddress fileValidatorHash
    pure $ mapMaybe f $ Map.toList utxos
  where
    f (oref, ot) = case getTypedDatum ot of
        Just d | txOutHasNFT (fileNFT d) (txOutTxOut ot) -> Just (oref, ot, d)
        _                                                -> Nothing

findFilesWhere :: forall w s. (FileDatum -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, FileDatum)]
findFilesWhere f = filter (\(_,_,d) -> f d) <$> findValidFiles

findFile :: forall w s. CurrencySymbol -> Contract w s Text (TxOutRef, TxOutTx, FileDatum)
findFile sym = do
    utxos <- findFilesWhere hasValidNFT
    case utxos of
        [(oref, ot, d)] -> do
            pkh <- pubKeyHash <$> Contract.ownPubKey
            when (hasNoAccess pkh d) $ throwError "user has no access to this file"
            pure (oref, ot, d)
        _               -> throwError "no valid file with that token"
  where
    nft = AssetClass (sym, fileTokenName)

    hasValidNFT :: FileDatum -> Bool
    hasValidNFT datum = fileNFT datum == nft

    hasNoAccess :: PubKeyHash -> FileDatum -> Bool
    hasNoAccess pkh FileDatum{..} = pkh `notElem` fAdmin

type FileStorageSchema
    =   Endpoint "create" CreateFileArgs
    .\/ Endpoint "close" ByteString
    .\/ Endpoint "check" ByteString
    .\/ Endpoint "list"  ()
    .\/ Endpoint "rename" RenameFileArgs
    .\/ Endpoint "set-admin" SetAdminArgs

data FileStorageObservable
    = Created FileIdent | Closed  FileIdent
    | Checked FileDatum
    | Listed  [FileIdent]
    | Renamed FileIdent
    | AdminSet FileIdent
    | Loading String
    deriving (Show, Generic, FromJSON, ToJSON)

type FileStorageEndpoint e a =
    Contract (Last (Either Text FileStorageObservable)) FileStorageSchema e a

endpoints :: FileStorageEndpoint Void ()
endpoints
    = create
    `select` close
    `select` check
    `select` list
    `select` rename
    `select` setAdmin
    >> endpoints

  where
    create :: FileStorageEndpoint Void ()
    create = handleErr $ do
        args <- endpoint @"create"
        tellLoading "create"
        Created <$> createFile args

    close :: FileStorageEndpoint Void ()
    close = handleErr $ do
        sym <- endpoint @"close"
        tellLoading "close"
        Closed <$> closeFile sym

    check :: FileStorageEndpoint Void ()
    check = handleErr $ do
        sym <- endpoint @"check"
        tellLoading "check"
        Checked <$> checkFile sym

    list :: FileStorageEndpoint Void ()
    list = handleErr $ do
        endpoint @"list"
        tellLoading "list"
        Listed <$> listFiles

    rename :: FileStorageEndpoint Void ()
    rename = handleErr $ do
        args <- endpoint @"rename"
        tellLoading "rename"
        Renamed <$> renameFile args

    setAdmin :: FileStorageEndpoint Void ()
    setAdmin = handleErr $ do
        args <- endpoint @"set-admin"
        tellLoading "set-admin"
        AdminSet <$> setFileAdmin args

    handleErr :: FileStorageEndpoint Text FileStorageObservable -> FileStorageEndpoint Void ()
    handleErr c = do
        e <- Contract.runError c
        case e of
            Left err  -> do
                tell $ Last $ Just $ Left err
                logError err
            Right obs ->
                tell $ Last $ Just $ Right obs

    tellLoading = tell . Last . Just . Right . Loading
