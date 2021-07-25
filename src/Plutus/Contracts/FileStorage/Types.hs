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

module Plutus.Contracts.FileStorage.Types
    ( FileDatum (..)
    , FileRedeemer (..)
    , FileBeginUploadArgs (..)
    , FileUploadShardArgs (..)
    , ShardDatum (..)
    ) where

import           Data.Aeson       (FromJSON, ToJSON)
import           GHC.Generics     (Generic)
import           Ledger
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), check)
import           Prelude          (Semigroup (..))
import qualified Prelude          as Haskell

import Plutus.Contracts.ScriptToken.Types (ScriptToken (..))

data FileDatum
    = FileDatum
        { fOwner         :: !PubKeyHash
        , fScriptToken   :: !ScriptToken
        , fShardSize     :: !Integer
        , fFileName      :: !ByteString
        , fAdmin         :: ![PubKeyHash]
        , fLastUpdatedBy :: !PubKeyHash
        , fUploadLock    :: !(Maybe ByteString)
        , fShardNFT      :: !(Maybe AssetClass)
        , fChecksum      :: !(Maybe ByteString)
        }
    deriving (Haskell.Show, Generic, FromJSON, ToJSON)

instance Eq FileDatum where
    {-# INLINABLE (==) #-}
    fd == fd' =
        fOwner         fd == fOwner         fd' &&
        fScriptToken   fd == fScriptToken   fd' &&
        fShardSize     fd == fShardSize     fd' &&
        fFileName      fd == fFileName      fd' &&
        fAdmin         fd == fAdmin         fd' &&
        fLastUpdatedBy fd == fLastUpdatedBy fd' &&
        fUploadLock    fd == fUploadLock    fd' &&
        fShardNFT      fd == fShardNFT      fd' &&
        fChecksum      fd == fChecksum      fd'

PlutusTx.makeLift ''FileDatum
PlutusTx.makeIsDataIndexed ''FileDatum [('FileDatum, 0)]

data FileBeginUploadArgs 
    = FileBeginUploadArgs
        { bupChecksum   :: ByteString
        , bupUploadLock :: ByteString
        }
    deriving (Haskell.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''FileBeginUploadArgs
PlutusTx.makeIsDataIndexed ''FileBeginUploadArgs [('FileBeginUploadArgs, 0)]

data FileUploadShardArgs 
    = FileUploadShardArgs
        { upShardNFT   :: AssetClass
        , upBytes      :: ByteString
        , upIsDone     :: Bool
        , upUploadLock :: ByteString
        }
    deriving (Haskell.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''FileUploadShardArgs
PlutusTx.makeIsDataIndexed ''FileUploadShardArgs [('FileUploadShardArgs, 0)]

data FileRedeemer
    = BeginUpload FileBeginUploadArgs
    | UploadShard FileUploadShardArgs
    | CancelUpload ByteString
    | Rename ByteString
    | SetAdmin [PubKeyHash]
    | Remove
    | Close
    deriving (Haskell.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''FileRedeemer
PlutusTx.makeIsDataIndexed ''FileRedeemer [('BeginUpload,  0)
                                          ,('UploadShard,  1)
                                          ,('CancelUpload, 2)
                                          ,('Rename,       3)
                                          ,('SetAdmin,     4)
                                          ,('Remove,       5)
                                          ,('Close,        6)
                                          ]

data ShardDatum
    = ShardDatum
        { shBytes   :: !ByteString
        , shNFT     :: !AssetClass
        , shNextNFT :: !(Maybe AssetClass)
        }
    deriving (Haskell.Show, Generic, FromJSON, ToJSON)

instance Eq ShardDatum where
    {-# INLINABLE (==) #-}
    shd == shd' =
        shBytes   shd == shBytes   shd' &&
        shNFT     shd == shNFT     shd' &&
        shNextNFT shd == shNextNFT shd'

PlutusTx.makeLift ''ShardDatum
PlutusTx.makeIsDataIndexed ''ShardDatum [('ShardDatum, 0)]
