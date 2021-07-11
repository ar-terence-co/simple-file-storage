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
    where

import           Control.Monad                      hiding (fmap)
import           Data.Aeson                         (FromJSON, ToJSON)
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
import           Prelude                            (Semigroup (..), String, Show (..))
import qualified Prelude                            as Haskell
import           Text.Printf          (printf)

import           Plutus.Contracts.FileStorage.File  (fileAddress, fileTypedValidator)
import           Plutus.Contracts.FileStorage.Types (FileDatum (..), FileRedeemer (..), ShardDatum (..),
                                                     FileBeginUploadArgs (..), FileUploadShardArgs (..))
import           Plutus.Contracts.FileStorage.Utils (txOutHasNFT, getTypedDatum)

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

createFile :: forall w s. ByteString -> Integer -> Contract w s Text (ByteString, CurrencySymbol)
createFile fileName shardSize = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    sym <- fmap Currency.currencySymbol $
           Contract.mapError (pack . show @Currency.CurrencyError) $
           Currency.mintContract pkh [(fileTokenName, 1)]
    let nft = AssetClass (sym, fileTokenName)
        d   = initialFileDatum pkh nft fileName shardSize
        v   = Value.assetClassValue nft 1
        tx  = Constraints.mustPayToTheScript d v
    ledgerTx <- Contract.submitTxConstraints fileTypedValidator tx
    void $ Contract.awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "created a file %s for token %s" (show fileName) (show nft)
    pure (fileName, sym)

closeFile :: forall w s. CurrencySymbol -> Contract w s Text (ByteString, CurrencySymbol)
closeFile sym = do
    (oref, ot, FileDatum{..}) <- findFile sym
    -- TODO: close shards as well
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let v        = Value.assetClassValue fNFT 1
        redeemer = Redeemer $ PlutusTx.toData Close
        lookups  = Constraints.typedValidatorLookups fileTypedValidator <>
                   Constraints.unspentOutputs (Map.singleton oref ot)   <>
                   Constraints.ownPubKeyHash pkh
        tx       = Constraints.mustSpendScriptOutput oref redeemer <>
                   Constraints.mustMintValue (negate v)
    ledgerTx <- Contract.submitTxConstraintsWith lookups tx
    void $ Contract.awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "closed the file %s for token %s" (show fFileName) (show fNFT)
    pure (fFileName, sym)

checkFile :: forall w s. CurrencySymbol -> Contract w s Text FileDatum
checkFile sym = do
    (_, _, datum) <- findFile sym
    pure datum

listFiles :: forall w s. Contract w s Text [(ByteString, CurrencySymbol)]
listFiles = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    map getFileIdent <$> findFilesWhere (isAdmin pkh)
  where
    isAdmin :: PubKeyHash -> FileDatum -> Bool
    isAdmin pkh FileDatum{..} = pkh `elem` fAdmin

    getFileIdent :: (TxOutRef, TxOutTx, FileDatum) -> (ByteString, CurrencySymbol)
    getFileIdent (_,_,FileDatum{..}) = (fFileName, fst $ unAssetClass fNFT)

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
    =   Endpoint "create" (ByteString, Integer)
    .\/ Endpoint "close" CurrencySymbol
    .\/ Endpoint "check" CurrencySymbol
    .\/ Endpoint "list"  ()

data FileStorageObservable 
    = Created (ByteString, CurrencySymbol) | Closed  (ByteString, CurrencySymbol)
    | Checked FileDatum
    | Listed  [(ByteString, CurrencySymbol)]
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
        (fileName, shardSize) <- endpoint @"create"
        Created <$> createFile fileName shardSize
    
    close :: FileStorageEndpoint Void ()
    close = handleErr $ do
        sym <- endpoint @"close"
        Closed <$> closeFile sym

    check :: FileStorageEndpoint Void ()
    check = handleErr $ do
        sym <- endpoint @"check"
        Checked <$> checkFile sym
    
    list :: FileStorageEndpoint Void ()
    list = handleErr $ do
        endpoint @"list"
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
