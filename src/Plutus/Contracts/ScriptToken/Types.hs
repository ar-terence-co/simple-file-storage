{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Plutus.Contracts.ScriptToken.Types
    ( ScriptTokenParams (..)
    , ScriptToken (..)
    ) where

import           Data.Aeson       (FromJSON, ToJSON)
import           GHC.Generics     (Generic)
import           Ledger
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), check)
import           Prelude          (Semigroup (..))
import qualified Prelude          as Haskell

data ScriptTokenParams
    = ScriptTokenParams
        { stpTxOutRef   :: !TxOutRef
        , stpScriptHash :: !ValidatorHash
        , stpTokenName  :: !TokenName
        }
    deriving (Haskell.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''ScriptTokenParams

data ScriptToken
    = ScriptToken
        { scriptTokenNFT      :: !AssetClass 
        , scriptTokenTxOutRef :: !TxOutRef  
        }
    deriving (Haskell.Show, Generic, FromJSON, ToJSON)

instance Eq ScriptToken where
    {-# INLINABLE (==) #-}
    st == st' =
        scriptTokenNFT      st == scriptTokenNFT      st' &&
        scriptTokenTxOutRef st == scriptTokenTxOutRef st'

PlutusTx.makeLift ''ScriptToken
PlutusTx.makeIsDataIndexed ''ScriptToken [('ScriptToken,  0)]
