{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), Options (..), ToJSON (..), defaultOptions,
                                                      genericParseJSON, genericToJSON)
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Ledger                              (pubKeyHash)
import qualified Plutus.Contracts.FileStorage        as FileStorage
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    logString "Starting PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    logString "Wallet PubKeyHashes"
    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        logString $ show w ++ ": " ++ show pkh

    void $ liftIO getLine

    logString "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    logBalances b

    shutdown
  where
    logString = Simulator.logString @(Builtin PABContracts)
    logBalances = Simulator.logBalances @(Builtin PABContracts)

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 5]]

data PABContracts = FileStorage
    deriving (Eq, Ord, Show, Generic)

-- Because PABContracts only has one constructor
instance ToJSON PABContracts where
    toJSON = genericToJSON defaultOptions { tagSingleConstructors = True }

-- Because PABContracts only has one constructor
instance FromJSON PABContracts where
    parseJSON = genericParseJSON defaultOptions { tagSingleConstructors = True }

instance Pretty PABContracts where
    pretty = viaShow

handlePABContracts ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin PABContracts))) effs
    )
    => ContractEffect (Builtin PABContracts)
    ~> Eff effs
handlePABContracts = Builtin.handleBuiltin getSchema getContract where
    getSchema = \case
        FileStorage -> Builtin.endpointsToSchemas @FileStorage.FileStorageSchema
    getContract = \case
        FileStorage -> SomeBuiltin (FileStorage.endpoints)

handlers :: SimulatorEffectHandlers (Builtin PABContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin PABContracts) [FileStorage]
    $ interpret handlePABContracts
