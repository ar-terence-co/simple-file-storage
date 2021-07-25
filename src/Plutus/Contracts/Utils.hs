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

module Plutus.Contracts.Utils 
    ( getTypedDatum
    , getTypedDatumFromInfo
    , txOutHasNFT
    , findInputWithNFT
    , findOutputWithNFT
    ) where

import           Control.Monad                      hiding (fmap)
import           Data.Map                           as Map
import           GHC.Generics                       (Generic)
import           Ledger
import qualified Ledger.Typed.Scripts               as Scripts
import           Ledger.Value                       as Value
import qualified PlutusTx
import           PlutusTx.Prelude                   hiding (Semigroup (..), check)
import           Prelude                            (Semigroup (..))
import qualified Prelude                            as Haskell

getTypedDatum :: PlutusTx.IsData a => TxOutTx -> Maybe a
getTypedDatum ot = do
    Datum d <- txOutTxDatum ot
    PlutusTx.fromData d  

{-# INLINABLE getTypedDatumFromInfo #-}
getTypedDatumFromInfo :: PlutusTx.IsData a => TxInfo -> TxOut -> Maybe a
getTypedDatumFromInfo info o = do
    dh      <- txOutDatum o
    Datum d <- dh `Ledger.findDatum` info
    PlutusTx.fromData d

{-# INLINABLE txOutHasNFT #-}
txOutHasNFT :: AssetClass -> TxOut -> Bool
txOutHasNFT nft o = Value.assetClassValueOf (txOutValue o) nft == 1

{-# INLINABLE findInputWithNFT #-}
findInputWithNFT :: AssetClass -> TxInfo -> Maybe TxOut
findInputWithNFT nft info =
    case ins of
        [i] -> Just i
        _   -> Nothing
  where
    ins = [i | i <- txInInfoResolved <$> txInfoInputs info, txOutHasNFT nft i]

{-# INLINABLE findOutputWithNFT #-}
findOutputWithNFT :: AssetClass -> TxInfo -> Maybe TxOut
findOutputWithNFT nft info = 
    case outs of
        [o] -> Just o
        _   -> Nothing
  where
    outs = [o | o <- txInfoOutputs info, txOutHasNFT nft o]
