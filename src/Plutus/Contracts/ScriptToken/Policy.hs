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

module Plutus.Contracts.ScriptToken.Policy
    ( scriptTokenPolicy
    , scriptTokenSymbol
    , scriptToken
    ) where

import           Control.Monad                      hiding (fmap)
import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)
import           Ledger
import qualified Ledger.Typed.Scripts               as Scripts
import           Ledger.Value                       as Value
import qualified PlutusTx
import           PlutusTx.Prelude                   hiding (Semigroup (..), check)
import           Prelude                            (Semigroup (..))
import qualified Prelude                            as Haskell

import           Plutus.Contracts.ScriptToken.Types (ScriptTokenParams (..), ScriptToken (..))
import           Plutus.Contracts.Utils             (findInputWithNFT, findOutputWithNFT)

{-# INLINABLE mkPolicy #-}
mkPolicy :: ScriptTokenParams -> () -> ScriptContext -> Bool
mkPolicy ScriptTokenParams{..} _ ctx
    | mintedAmount == 1 =
        traceIfFalse "the token reference utxo should be consumed" refIsInput         &&
        traceIfFalse "only the script can receive the nft"         scriptOutputHasNFT 
    | mintedAmount == (-1) =
        traceIfFalse "the script must own the nft" scriptInputHasNFT
    | otherwise =
        traceError "only 1 token can be minted or burned"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    refIsInput :: Bool
    refIsInput = any (\i -> txInInfoOutRef i == stpTxOutRef) $ txInfoInputs info

    scriptInputHasNFT :: Bool
    scriptInputHasNFT = case findInputWithNFT nft info of
        Nothing -> traceError "no input with nft"
        Just i  -> txOutAddress i == stpScriptAddress

    scriptOutputHasNFT :: Bool
    scriptOutputHasNFT = case findOutputWithNFT nft info of
        Nothing -> traceError "no output with nft"
        Just o  -> txOutAddress o == stpScriptAddress

    stpScriptAddress :: Address
    stpScriptAddress = Ledger.scriptHashAddress stpScriptHash

    mintedAmount :: Integer
    mintedAmount = Value.assetClassValueOf (txInfoForge info) nft

    nft :: AssetClass
    nft = AssetClass (Ledger.ownCurrencySymbol ctx, stpTokenName)

scriptTokenPolicy :: ScriptTokenParams -> Scripts.MintingPolicy
scriptTokenPolicy params = Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode params

scriptTokenSymbol :: ScriptTokenParams -> CurrencySymbol
scriptTokenSymbol = scriptCurrencySymbol . scriptTokenPolicy

scriptToken :: ScriptTokenParams -> ScriptToken
scriptToken params@ScriptTokenParams{..}
    = ScriptToken
        { scriptTokenNFT      = AssetClass (sym, stpTokenName)
        , scriptTokenTxOutRef = stpTxOutRef 
        }
  where
    sym = scriptTokenSymbol params
