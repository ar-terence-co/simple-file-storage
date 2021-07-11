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

module Plutus.Contracts.FileStorage.File 
    ( fileTypedValidator
    , fileValidator
    , fileAddress
    ) where

import           Control.Monad                      hiding (fmap)
import           GHC.Generics                       (Generic)
import           Ledger
import qualified Ledger.Typed.Scripts               as Scripts
import           Ledger.Value                       as Value
import qualified PlutusTx
import           PlutusTx.Prelude                   hiding (Semigroup (..), check)
import           Prelude                            (Semigroup (..))
import qualified Prelude                            as Haskell

import           Plutus.Contracts.FileStorage.Types (FileDatum (..), FileRedeemer (..), ShardDatum (..),
                                                     FileBeginUploadArgs (..), FileUploadShardArgs (..))
import           Plutus.Contracts.FileStorage.Utils (getTypedDatumFromInfo, txOutHasNFT,
                                                     findInputWithNFT, findOutputWithNFT)

{-# INLINABLE txCommons #-}
txCommons :: ScriptContext -> (TxInfo, TxOut, TxOut, FileDatum)
txCommons ctx = (info, ownInput, ownOutput, ownOutputDatum)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case Ledger.findOwnInput ctx of
        Nothing -> traceError "input file script missing"
        Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case Ledger.getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "exactly one output file script allowed"

    ownOutputDatum :: FileDatum
    ownOutputDatum = case maybeDatum of
        Nothing -> traceError "invalid output datum"
        Just datum -> datum
      where 
        maybeDatum = getTypedDatumFromInfo info ownOutput

{-# INLINABLE validateCommon #-}
validateCommon :: FileDatum -> ScriptContext -> Bool 
validateCommon datum ctx = 
    traceIfFalse "missing nft from input script"                     ownInputHasNFT      &&         
    traceIfFalse "missing nft from output script"                    ownOutputHasNFT     && 
    traceIfFalse "script value was changed"                          hasUnchangedValue   &&                                 
    traceIfFalse "the updater should sign the tx"                    updaterIsSignatory  &&
    traceIfFalse "the updater should be an admin"                    updaterIsAdmin      &&
    traceIfFalse "invalid input script datum. please close the file" hasValidInputDatum  &&
    traceIfFalse "invalid output script datum"                       hasValidOutputDatum
  where
    (info, ownInput, ownOutput, ownOutputDatum) = txCommons ctx
    nft = fNFT datum

    ownInputHasNFT :: Bool
    ownInputHasNFT = txOutHasNFT nft ownInput

    ownOutputHasNFT :: Bool
    ownOutputHasNFT = txOutHasNFT nft ownOutput

    hasUnchangedValue :: Bool
    hasUnchangedValue = txOutValue ownInput == txOutValue ownOutput

    updaterIsSignatory :: Bool
    updaterIsSignatory = fLastUpdatedBy ownOutputDatum `elem` txInfoSignatories info

    updaterIsAdmin :: Bool
    updaterIsAdmin = fLastUpdatedBy ownOutputDatum `elem` fAdmin datum
    
    hasValidInputDatum :: Bool
    hasValidInputDatum = isDatumValid datum

    hasValidOutputDatum :: Bool
    hasValidOutputDatum = isDatumValid ownOutputDatum 

    isDatumValid :: FileDatum -> Bool
    isDatumValid d = 
        traceIfFalse "datum invalid: filename cannot be empty" (fFileName d /= emptyByteString) &&
        traceIfFalse "datum invalid: owner should be admin"    (fOwner d `elem` fAdmin d)

{-# INLINABLE validateShard #-}
validateShard :: FileDatum -> ScriptContext -> Bool 
validateShard datum ctx = case fShardNFT datum of
    Nothing -> True 
    Just nft -> 
        traceIfFalse "nft in file and input shard dont match" shInputHasValidNFT 
      where
        (info, _, _, _) = txCommons ctx

        shInput :: TxOut
        shInput = case findInputWithNFT nft info of
            Nothing -> traceError "no input with shard nft"
            Just i -> i

        shInputDatum :: ShardDatum
        shInputDatum = case maybeDatum of
            Nothing -> traceError "invalid input shard datum"
            Just datum -> datum
          where 
            maybeDatum = getTypedDatumFromInfo info shInput

        shInputHasValidNFT :: Bool
        shInputHasValidNFT = shNFT shInputDatum == nft

{-# INLINABLE validateUploading #-}
validateUploading :: FileDatum -> Maybe ByteString -> Bool
validateUploading datum uploadLock = case uploadLock of
    Just lock ->
        traceIfFalse "redeemer should have correct upload lock" hasUploadLock
    Nothing -> 
        traceIfFalse "cannot redeem while uploading" hasUploadLock
  where
    hasUploadLock :: Bool
    hasUploadLock = fUploadLock datum == uploadLock

{-# INLINABLE validateBeginUpload #-}
validateBeginUpload :: FileDatum -> FileBeginUploadArgs -> ScriptContext -> Bool
validateBeginUpload datum FileBeginUploadArgs{..} ctx = 
    validateCommon datum ctx &&
    validateShard datum ctx  &&
    validateUploading datum Nothing &&
    traceIfFalse "invalid `begin-upload` datum changes: \
                 \checksum and lock should be set, \
                 \shard nft should be unset, \
                 \other changes invalid." hasValidChanges
  where
    (_, _, _, ownOutputDatum) = txCommons ctx

    hasValidChanges :: Bool
    hasValidChanges = 
        ownOutputDatum == datum
            { fChecksum = Just bupChecksum
            , fUploadLock = Just bupUploadLock
            , fShardNFT = Nothing
            , fLastUpdatedBy = fLastUpdatedBy ownOutputDatum
            }

{-# INLINABLE validateUploadShard #-}
validateUploadShard :: FileDatum -> FileUploadShardArgs -> ScriptContext -> Bool
validateUploadShard datum FileUploadShardArgs{..} ctx = 
    validateCommon datum ctx             &&
    validateUploading datum (Just upUploadLock) &&
    traceIfFalse "invalid Shard datum"               shOutputHasValidDatum &&
    traceIfFalse "should forge nft for output shard" shOutputNFTForged     &&
    traceIfFalse "invalid `upload-shard` datum changes: \
                 \shard nft should be set, \
                 \lock should be unlocked when done. \
                 \updater should not change. \
                 \other changes invalid." hasValidChanges
  where
    (info, _, _, ownOutputDatum) = txCommons ctx

    shOutput :: TxOut
    shOutput = case findInputWithNFT upShardNFT info of
        Nothing -> traceError "no output with shard nft"
        Just i -> i

    shOutputDatum :: ShardDatum
    shOutputDatum = case maybeDatum of
        Nothing -> traceError "invalid output shard datum"
        Just datum -> datum
      where 
        maybeDatum = getTypedDatumFromInfo info shOutput

    shOutputHasValidDatum :: Bool
    shOutputHasValidDatum = 
        shOutputDatum == ShardDatum
            { shBytes = upBytes
            , shNFT = upShardNFT
            , shNextNFT = fShardNFT datum
            }

    shOutputNFTForged :: Bool 
    shOutputNFTForged = Value.assetClassValueOf (txInfoForge info) upShardNFT == 1

    hasValidChanges :: Bool
    hasValidChanges = 
        ownOutputDatum == datum
            { fShardNFT = Just upShardNFT
            , fUploadLock = lockChange
            }         

    lockChange :: Maybe ByteString
    lockChange = if upIsDone then Nothing else fUploadLock datum

{-# INLINABLE validateCancelUpload #-}
validateCancelUpload :: FileDatum -> ByteString -> ScriptContext -> Bool
validateCancelUpload datum lock ctx = 
    validateCommon datum ctx     &&
    validateUploading datum (Just lock) &&
    traceIfFalse "invalid `cancel-upload` datum changes: \
                 \lock should be unlocked. \
                 \other changes invalid." hasValidChanges
  where
    (_, _, _, ownOutputDatum) = txCommons ctx

    hasValidChanges :: Bool
    hasValidChanges = 
        ownOutputDatum == datum
            { fUploadLock = Nothing
            , fLastUpdatedBy = fLastUpdatedBy ownOutputDatum
            }    

{-# INLINABLE validateRename #-}
validateRename :: FileDatum -> ByteString -> ScriptContext -> Bool
validateRename datum newName ctx = 
    validateCommon datum ctx &&
    validateUploading datum Nothing &&
    traceIfFalse "invalid `rename` datum changes: \
                 \name should be set. \
                 \other changes invalid." hasValidChanges
  where
    (_, _, _, ownOutputDatum) = txCommons ctx

    hasValidChanges :: Bool
    hasValidChanges = 
        ownOutputDatum == datum
            { fFileName = newName
            , fLastUpdatedBy = fLastUpdatedBy ownOutputDatum
            }       

{-# INLINABLE validateSetAdmin #-}
validateSetAdmin :: FileDatum -> [PubKeyHash] -> ScriptContext -> Bool
validateSetAdmin datum pkhs ctx = 
    validateCommon datum ctx &&
    validateUploading datum Nothing &&
    traceIfFalse "invalid `set-admin` datum changes: \
                 \admin should be set. \
                 \other changes invalid." hasValidChanges
  where
    (_, _, _, ownOutputDatum) = txCommons ctx

    hasValidChanges :: Bool
    hasValidChanges = 
        ownOutputDatum == datum
            { fAdmin = pkhs
            , fLastUpdatedBy = fLastUpdatedBy ownOutputDatum
            }    

{-# INLINABLE validateRemove #-}
validateRemove :: FileDatum -> ScriptContext -> Bool
validateRemove datum ctx = 
    validateCommon datum ctx &&
    validateShard datum ctx  &&
    validateUploading datum Nothing &&
    traceIfFalse "invalid `remove` datum changes: \
                 \checksum and shard should be unset. \
                 \other changes invalid." hasValidChanges
  where
    (_, _, _, ownOutputDatum) = txCommons ctx

    hasValidChanges :: Bool
    hasValidChanges = 
        ownOutputDatum == datum
            { fChecksum = Nothing
            , fShardNFT = Nothing
            , fLastUpdatedBy = fLastUpdatedBy ownOutputDatum
            }    

{-# INLINABLE validateClose #-}
validateClose :: FileDatum -> ScriptContext -> Bool 
validateClose datum ctx = 
    traceIfFalse "only the owner can close the file"   hasOwnerSignatory &&
    traceIfFalse "no continuing outputs on file close" noOwnOutputs      &&
    traceIfFalse "script nft must be burned"           nftBurned      
  where
    (info, _, _, _) = txCommons ctx
    nft = fNFT datum
    
    hasOwnerSignatory :: Bool 
    hasOwnerSignatory = fOwner datum `elem` txInfoSignatories info

    noOwnOutputs :: Bool
    noOwnOutputs = null $ Ledger.getContinuingOutputs ctx

    nftBurned :: Bool
    nftBurned = 
        Value.assetClassValueOf (txInfoForge info) nft == negate 1

{-# INLINABLE mkValidator #-}
mkValidator :: FileDatum -> FileRedeemer -> ScriptContext -> Bool 
mkValidator datum (BeginUpload args)  ctx = validateBeginUpload  datum args    ctx
mkValidator datum (UploadShard args)  ctx = validateUploadShard  datum args    ctx
mkValidator datum (CancelUpload lock) ctx = validateCancelUpload datum lock    ctx
mkValidator datum (Rename newName)    ctx = validateRename       datum newName ctx
mkValidator datum (SetAdmin pkhs)     ctx = validateSetAdmin     datum pkhs    ctx
mkValidator datum Remove              ctx = validateRemove       datum         ctx
mkValidator datum Close               ctx = validateClose        datum         ctx

data Filing
instance Scripts.ValidatorTypes Filing where
    type instance DatumType Filing = FileDatum
    type instance RedeemerType Filing = FileRedeemer

fileTypedValidator :: Scripts.TypedValidator Filing
fileTypedValidator = Scripts.mkTypedValidator @Filing
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

fileValidator :: Scripts.Validator 
fileValidator = Scripts.validatorScript fileTypedValidator

fileAddress :: Address 
fileAddress = Ledger.scriptAddress fileValidator
