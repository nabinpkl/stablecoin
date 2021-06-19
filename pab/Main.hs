{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main(main) where

import           Control.Monad                           (forM_, void, when)
import           Control.Monad.Freer                     (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error               (Error)
import           Control.Monad.Freer.Extras.Log          (LogMsg)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , defaultOptions, Options(..), Result(Success),fromJSON)
import qualified Data.Map.Strict                         as Map
import qualified Data.Monoid                             as Monoid
import qualified Data.Semigroup                          as Semigroup
import           Data.Text                               (Text)
import           Data.Text.Prettyprint.Doc               (Pretty (..), viaShow)
import           GHC.Generics                            (Generic)
import           Ledger.Ada                              (adaSymbol, adaToken, adaValueOf,lovelaceValueOf)
import           Plutus.Contract                         hiding (when)
import           Plutus.PAB.Effects.Contract             (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin     (Builtin, SomeBuiltin (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin     as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg         (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                    (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                    as Simulator
import           Plutus.PAB.Types                        (PABError (..))
import qualified Plutus.PAB.Webserver.Server             as PAB.Server
import           Prelude                                 hiding (init)
import           Wallet.Emulator.Types                   (Wallet (..), walletPubKey)
import           Wallet.Types                        (ContractInstanceId (..))
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import qualified Plutus.Contracts.Currency as Currency

import qualified Plutus.Contracts.CoinsStateMachine as StableCoin
import qualified Plutus.Contracts.OracleContract.Core       as OracleCore

main :: IO ()
main =  
    void $ Simulator.runSimulationWith handlers $ do 
    Simulator.logString @(Builtin StableContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidOracle <- Simulator.activateContract (Wallet 1) OracleContract
    oracle <- waitForLast cidOracle


    Simulator.logString @(Builtin StableContracts) "Called oracle contract"

    w1cid <- Simulator.activateContract (Wallet 1) StableContract        
    Simulator.logString @(Builtin StableContracts) "Contract starting by wallet 1"

    --TODO start on init contract instead of endpoint
    let i = 1 :: Integer
    _ <- Simulator.callEndpointOnInstance w1cid "start" i

    -- forM_ wallets $ \w -> do
    --         cid <- Simulator.activateContract w  StableContract
    --         liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid
    
    void $ liftIO getLine
    
    Simulator.logString @(Builtin StableContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin StableContracts) b

    shutdown


waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Monoid.Last (Just x)) -> Just x
        _                       -> Nothing

data StableContracts = StableContract | OracleContract
    deriving (Eq, Ord, Show, Generic)

instance ToJSON StableContracts where
  toJSON = genericToJSON defaultOptions {
             tagSingleConstructors = True }
instance FromJSON StableContracts where
  parseJSON = genericParseJSON defaultOptions {
             tagSingleConstructors = True }

instance Pretty StableContracts where
    pretty = viaShow

wallets :: [Wallet]
wallets = [Wallet i | i <- [2 .. 4]]

handleTokenContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin StableContracts))) effs
    )
    => ContractEffect (Builtin StableContracts)
    ~> Eff effs
handleTokenContract = Builtin.handleBuiltin getSchema getContract where
    getSchema = \case
      StableContract -> Builtin.endpointsToSchemas @(StableCoin.BankStateSchema .\\ BlockchainActions)
      OracleContract         -> Builtin.endpointsToSchemas @(OracleCore.OracleSchema        .\\ BlockchainActions)
    getContract = \case
      StableContract -> SomeBuiltin StableCoin.endpoints
      OracleContract         -> SomeBuiltin $ OracleCore.runOracle

handlers :: SimulatorEffectHandlers (Builtin StableContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin StableContracts) [] 
    $ interpret handleTokenContract
