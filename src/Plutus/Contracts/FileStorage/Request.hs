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

import           Plutus.Contracts.FileStorage.File  (fileAddress, fileTypedValidator)
import           Plutus.Contracts.FileStorage.Types (FileBeginUploadArgs (..), FileDatum (..), FileRedeemer (..),
                                                     FileUploadShardArgs (..), ShardDatum (..))
import           Plutus.Contracts.FileStorage.Utils (getTypedDatum, txOutHasNFT)

data CreateFileArgs
    = CreateFileArgs
        { crfFileName  :: String
        , crfShardSize :: Integer
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

initialFileDatum :: PubKeyHash -> AssetClass -> ByteString -> Integer -> FileDatum
initialFileDatum pkh nft fileName shardSize
    = FileDatum
        { fOwner         = pkh
        , fNFT           = nft
        , fShardSize     = shardSize
        , fFileName      = fileName
        , fAdmin         = [pkh]
        , fLastUpdatedBy = pkh
        , fUploadLock    = Nothing
        , fShardNFT      = Nothing
        , fChecksum      = Nothing
        }

createFile :: forall w s. CreateFileArgs -> Contract w s Text FileIdent
createFile CreateFileArgs{..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    cs <- fmap Currency.currencySymbol $
           Contract.mapError (pack . show @Currency.CurrencyError) $
           Currency.mintContract pkh [(fileTokenName, 1)]
    let nft = AssetClass (cs, fileTokenName)
        d   = initialFileDatum pkh nft bsFileName crfShardSize
        v   = Value.assetClassValue nft 1
        tx  = Constraints.mustPayToTheScript d v
    ledgerTx <- Contract.submitTxConstraints fileTypedValidator tx
    void $ Contract.awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "created a file %s for token %s" (show crfFileName) (show nft)
    pure $ FileIdent crfFileName (unCurrencySymbol cs)
  where
    bsFileName = BSU.fromString crfFileName

closeFile :: forall w s. ByteString -> Contract w s Text FileIdent
closeFile sym = do
    (oref, ot, FileDatum{..}) <- findFile $ CurrencySymbol sym
    -- TODO: close shards as well
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let fileName = BSU.toString fFileName
        v        = Value.assetClassValue fNFT 1
        redeemer = Redeemer $ PlutusTx.toData Close
        lookups  = Constraints.typedValidatorLookups fileTypedValidator <>
                   Constraints.unspentOutputs (Map.singleton oref ot)   <>
                   Constraints.ownPubKeyHash pkh
        tx       = Constraints.mustSpendScriptOutput oref redeemer <>
                   Constraints.mustMintValue (negate v)
    ledgerTx <- Contract.submitTxConstraintsWith lookups tx
    void $ Contract.awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "closed the file %s for token %s" (show fileName) (show fNFT)
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
    getFileIdent (_,_,FileDatum{..}) = FileIdent (BSU.toString fFileName) sym
      where
        sym = unCurrencySymbol $ fst $ unAssetClass fNFT

upload = Haskell.undefined

cancelUpload = Haskell.undefined

remove = Haskell.undefined

rename = Haskell.undefined

setAdmin = Haskell.undefined

findValidFiles :: forall w s. Contract w s Text [(TxOutRef, TxOutTx, FileDatum)]
findValidFiles = do
    utxos <- Contract.utxoAt fileAddress
    pure $ mapMaybe f $ Map.toList utxos
  where
    f (oref, ot) = case getTypedDatum ot of
        Just d | txOutHasNFT (fNFT d) (txOutTxOut ot) -> Just (oref, ot, d)
        _                                             -> Nothing

findFilesWhere :: forall w s. (FileDatum -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, FileDatum)]
findFilesWhere f = filter (\(_,_,d) -> f d) <$> findValidFiles

findFile :: forall w s. CurrencySymbol -> Contract w s Text (TxOutRef, TxOutTx, FileDatum)
findFile sym = do
    utxos <- findFilesWhere hasValidNFT
    case utxos of
        [(oref, ot, d)] -> pure (oref, ot, d)
        _               -> throwError "no valid file with that token"
  where
    nft = AssetClass (sym, fileTokenName)

    hasValidNFT :: FileDatum -> Bool
    hasValidNFT datum = fNFT datum == nft

type FileStorageSchema
    =   Endpoint "create" CreateFileArgs
    .\/ Endpoint "close" ByteString
    .\/ Endpoint "check" ByteString
    .\/ Endpoint "list"  ()

data FileStorageObservable
    = Created FileIdent | Closed  FileIdent
    | Checked FileDatum
    | Listed  [FileIdent]
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
    >> endpoints

  where
    create :: FileStorageEndpoint Void ()
    create = handleErr $ do
        args <- endpoint @"create"
        tell $ Last $ Just $ Right $ Loading "create"
        Created <$> createFile args

    close :: FileStorageEndpoint Void ()
    close = handleErr $ do
        sym <- endpoint @"close"
        tell $ Last $ Just $ Right $ Loading "close"
        Closed <$> closeFile sym

    check :: FileStorageEndpoint Void ()
    check = handleErr $ do
        sym <- endpoint @"check"
        tell $ Last $ Just $ Right $ Loading "check"
        Checked <$> checkFile sym

    list :: FileStorageEndpoint Void ()
    list = handleErr $ do
        endpoint @"list"
        tell $ Last $ Just $ Right $ Loading "list"
        Listed <$> listFiles

    handleErr :: FileStorageEndpoint Text FileStorageObservable -> FileStorageEndpoint Void ()
    handleErr c = do
        e <- Contract.runError c
        case e of
            Left err  -> do
                tell $ Last $ Just $ Left err
                logError err
            Right obs ->
                tell $ Last $ Just $ Right obs
