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

module Plutus.Contracts.FileStorage.Shard
    ( shardTypedValidator
    , shardValidator
    , shardAddress
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

import           Plutus.Contracts.FileStorage.Types (FileDatum(..), ShardDatum(..))
import           Plutus.Contracts.FileStorage.Utils (getTypedDatumFromInfo, findInputWithNFT, txOutHasNFT)


{-# INLINABLE mkValidator #-}
mkValidator :: AssetClass -> ShardDatum -> () -> ScriptContext -> Bool
mkValidator fNFT ShardDatum{..} _ ctx =
    traceIfFalse "input shard does not have its NFT"           ownInputHasNFT        &&
    traceIfFalse "next shard input is an invalid script"       nextShardIsSameScript &&
    traceIfFalse "at least one signatory must be a file admin" hasAdminSignatories   &&
    traceIfFalse "no output shards allowed"                    noShardOutputs        &&
    traceIfFalse "shard nft must be burned"                    nftBurned         
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case Ledger.findOwnInput ctx of
        Nothing -> traceError "input shard missing"
        Just i  -> txInInfoResolved i

    fInput :: TxOut
    fInput = case findInputWithNFT fNFT info of
        Nothing -> traceError "no input with file nft"
        Just i -> i

    fInputDatum :: FileDatum
    fInputDatum = case maybeDatum of
        Nothing -> traceError "invalid input file script datum"
        Just datum -> datum
      where 
        maybeDatum = getTypedDatumFromInfo info fInput

    nextShInput :: Maybe TxOut
    nextShInput = case shNextNFT of
        Nothing -> Nothing
        Just nft -> case findInputWithNFT nft info of
            Nothing -> traceError "no input with next shard nft"
            Just i -> Just i

    ownInputHasNFT :: Bool
    ownInputHasNFT = txOutHasNFT shNFT ownInput

    hasAdminSignatories :: Bool
    hasAdminSignatories = any (`elem` admin) sigs
      where
        sigs = txInfoSignatories info
        admin = fAdmin fInputDatum

    noShardOutputs :: Bool
    noShardOutputs = null $ Ledger.getContinuingOutputs ctx

    nextShardIsSameScript :: Bool
    nextShardIsSameScript = case nextShInput of
        Nothing -> True
        Just oref -> txOutAddress oref == txOutAddress ownInput

    nftBurned :: Bool
    nftBurned = 
        Value.assetClassValueOf (txInfoForge info) shNFT == negate 1

data Sharding
instance Scripts.ValidatorTypes Sharding where
    type instance DatumType Sharding = ShardDatum
    type instance RedeemerType Sharding = ()

shardTypedValidator :: AssetClass -> Scripts.TypedValidator Sharding
shardTypedValidator = Scripts.mkTypedValidatorParam @Sharding
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

shardValidator :: AssetClass -> Scripts.Validator 
shardValidator = Scripts.validatorScript . shardTypedValidator

shardAddress :: AssetClass -> Address 
shardAddress = Ledger.scriptAddress . shardValidator
