{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Plutus.Contracts.Coins.Endpoints
  ( 
    BankStateSchema,
    coinsEndpoints,
  )
where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..))
import qualified Prelude                   as Prelude

import Plutus.Contracts.Coins.Types
import Plutus.Contracts.Oracle.Core
import qualified Plutus.Contract.StateMachine     as SM
import           Plutus.Contracts.Coins.CoinsStateMachine
import qualified Plutus.Contracts.Utils.StateMachine as SmUtil

import           Plutus.Contract.StateMachine     (SMContractError, StateMachineClient (..))
import           Playground.TH                     (mkSchemaDefinitions)
import           Ledger.Typed.Tx              (TypedScriptTxOut (..))
import qualified Data.Aeson.Types as Types
import Data.Aeson (toJSON)
import           Ledger.AddressMap                 (UtxoMap)

-- Get forwarding monteragy policy script hash from state machine instance
forwardMPS :: StateMachineClient CoinsMachineState BankInput -> MonetaryPolicyHash
forwardMPS StateMachineClient {scInstance} = Scripts.forwardingMonetaryPolicyHash $ SM.typedValidator scInstance

--Initial state of state machine to be set to all values to 0 with mps from script instance
initialState :: StateMachineClient CoinsMachineState BankInput -> CoinsMachineState
initialState smClient =
  CoinsMachineState
    { baseReserveAmount = 0,
      stableCoinAmount = 0,
      reserveCoinAmount = 0,
      policyScript = forwardMPS smClient,
      bankFee = 1 % 100,
      contractStatus = Running
    }

--Helper to convert SM contract error to Text error
mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . Prelude.show

--TODO check for handling multiple start call
--Endpoint to start the stable coint contract with  parameters supplied to banks
start :: HasBlockchainActions s => BankParam -> Integer -> Contract w s Text ()
start bankParam _ = do
  let client = machineClient (scriptInstance bankParam) bankParam
  void $ mapError' $ SM.runInitialise client (initialState client) mempty

--TODO handle error
--Run step function to construct lookups and execute run step of state machine
-- Specific input to state machine i.e either minting or redeeming of coins is passed 
smRunStep :: HasBlockchainActions s => BankParam -> BankInputAction -> Contract w s Text ()
smRunStep bankParam@BankParam{oracleParam} bankInputAction = do
  let client = machineClient (scriptInstance bankParam) bankParam

  oracle <- findOracle oracleParam
  
  case oracle of
    Nothing -> logInfo @Prelude.String "Oracle not found"
    Just (oref, o, x) -> do      
      let lookups = Constraints.unspentOutputs (Map.singleton oref o)
                    <> Constraints.otherScript (oracleValidator oracleParam)               
                                 
          input = BankInput bankInputAction (oref, txOutTxOut o, x)
              

      result <-  mapError' $ SmUtil.runStepWith client input lookups
      
      case result of
        SM.TransitionFailure e -> do
          void $ logInfo @Prelude.String $ "Transistion Faliure "
          throwError "Transistion Faliure"
        _ -> do
          void $ logInfo @Prelude.String $ "Endpoint call completed " ++ Prelude.show bankInputAction

--TODO check for validation in offchain
-- Contract endpoint for minting of stable coin
mintStableCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
mintStableCoin bankParam@BankParam{oracleParam} EndpointInput{tokenAmount} = smRunStep bankParam $ MintStableCoin tokenAmount

-- Contract endpoint for redeeming of stable coin to get ada back at current stable coin rate
redeemStableCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
redeemStableCoin bankParam EndpointInput{tokenAmount} = smRunStep bankParam $ RedeemStableCoin tokenAmount

-- Contract endpoint for minting of reserve coin
mintReserveCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
mintReserveCoin bankParam EndpointInput{tokenAmount} = smRunStep bankParam $ MintReserveCoin tokenAmount

-- Contract endpoint for redeeming of resever coin  to get ada back at current reserve rate
redeemReserveCoin :: HasBlockchainActions s => BankParam -> EndpointInput -> Contract w s Text ()
redeemReserveCoin bankParam EndpointInput{tokenAmount} = smRunStep bankParam $ RedeemReserveCoin tokenAmount

updateBankFee :: HasBlockchainActions s => BankParam -> BankFeeInput -> Contract w s Text ()
updateBankFee bankParam BankFeeInput{percentIntValue} = do
  let client = machineClient (scriptInstance bankParam) bankParam
      input = UpdateBankFee percentIntValue
      
  result <- mapError' $ SM.runStep client input
  case result of
    SM.TransitionFailure e -> do
      void $ logInfo @Prelude.String $ "Transistion Faliure "
      throwError "Transistion Faliure"
    _ -> do
      void $ logInfo @Prelude.String $ "Endpoint call completed " ++ Prelude.show input

updateContractStatus :: HasBlockchainActions s => BankParam -> ContractStatusInput -> Contract w s Text ()
updateContractStatus bankParam ContractStatusInput{shouldPause} = do
  let client = machineClient (scriptInstance bankParam) bankParam
      input = UpdateContractStatus shouldPause
              
  result <- mapError' $ SM.runStep client input

  case result of
    SM.TransitionFailure e -> do
      void $ logInfo @Prelude.String $ "Transistion Faliure Couldn't proceed further."
      throwError "Transistion Faliure"
    _ -> do
      void $ logInfo @Prelude.String $ "Endpoint call completed " ++ Prelude.show input


--Endpoint definitions availabe for the stable coin contract
type BankStateSchema =
  BlockchainActions
    .\/ Endpoint "start" Integer
    .\/ Endpoint "mintStableCoin" EndpointInput
    .\/ Endpoint "redeemStableCoin" EndpointInput
    .\/ Endpoint "mintReserveCoin" EndpointInput
    .\/ Endpoint "redeemReserveCoin" EndpointInput
    .\/ Endpoint "updateBankFee" BankFeeInput
    .\/ Endpoint "updateContractStatus" ContractStatusInput

    .\/ Endpoint "funds" Prelude.String
    .\/ Endpoint "currentState" Prelude.String
    .\/ Endpoint "currentRates" Prelude.String


coinsEndpoints :: BankParam -> Contract [Types.Value] BankStateSchema Text ()
coinsEndpoints bankParam = coinsContract bankParam >> coinsEndpoints bankParam


--TODO writer value [Types.Value]
--Starting point of the contract which combines all the endpoints to be available for call
coinsContract :: BankParam -> Contract [Types.Value] BankStateSchema Text ()
coinsContract bankParam = handleError handler (void selections)
  where 
    selections=
      ( 
        start'
          `select` mintStableCoin'
          `select` redeemStableCoin'
          `select` mintReserveCoin'
          `select` redeemReserveCoin'
          `select` updateBankFee'
          `select` updateContractStatus'

          `select` ownFunds'
          `select` currentState'
          `select` currentRates'
      )
    --TODO handle state for multiple start endpoint call
    start' = endpoint @"start" >>= start bankParam
    mintStableCoin' = endpoint @"mintStableCoin" >>= mintStableCoin bankParam
    redeemStableCoin' = endpoint @"redeemStableCoin" >>= redeemStableCoin bankParam
    mintReserveCoin' = endpoint @"mintReserveCoin" >>= mintReserveCoin bankParam
    redeemReserveCoin' = endpoint @"redeemReserveCoin" >>= redeemReserveCoin bankParam
    updateBankFee' = endpoint @"updateBankFee" >>= updateBankFee bankParam
    updateContractStatus' = endpoint @"updateContractStatus" >>= updateContractStatus bankParam
    
    ownFunds' = endpoint @"funds" >> ownFunds bankParam
    currentState' = endpoint @"currentState" >> currentCoinsMachineState bankParam
    currentRates' = endpoint @"currentRates" >> currentRates bankParam

    handler :: Prelude.Show a => a -> Contract w s e ()
    handler e = do
        Contract.logError $ Prelude.show e


--Endpoint for getting current funds held in a users public key
ownFunds:: HasBlockchainActions s => BankParam -> Contract [Types.Value ] s Text  ()
ownFunds _ = do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut Prelude.<$> utxos
    logInfo @Prelude.String $ "own funds: " ++ Prelude.show (flattenValue v)
    tell [ toJSON v]

--Helper fucntion for getting current state of state machine
getCurrentState :: ( SM.AsSMContractError e
    , HasUtxoAt schema)
    => BankParam -> Contract w schema e (Maybe (SM.OnChainState CoinsMachineState BankInput, UtxoMap))
getCurrentState bankParam = do
  let client = machineClient (scriptInstance bankParam) bankParam
  SM.getOnChainState client

--Endpoint for getting current state of state machine i.e current tokesn supply, base reserves etc.
currentCoinsMachineState:: HasBlockchainActions s => BankParam -> Contract [Types.Value ] s Text  ()
currentCoinsMachineState bankParam = do
  currentStateVal <- mapError' $ getCurrentState bankParam
  case currentStateVal of
    Just ((TypedScriptTxOut{tyTxOutData=state},_),_) -> do
        logInfo @Prelude.String $ "Current state: " ++ Prelude.show state
        let stateResponse = StateResponse{
          currentCoinsState = state
        }
        tell [toJSON stateResponse]
    Nothing -> logWarn @Prelude.String $ "Current state is not present yet."

--Endpoint for getting combined rates of peg, stable coin rate and reserve coin rate
currentRates :: HasBlockchainActions s => BankParam -> Contract [Types.Value ] s Text  ()
currentRates bankParam = do
  oracle <- findOracle $ oracleParam bankParam
  case oracle of
    Nothing -> logWarn @Prelude.String "Oracle not found"
    Just (oref, o, rate) -> do
      currentStateVal <- mapError' $ getCurrentState bankParam
      case currentStateVal of
          Just ((TypedScriptTxOut{tyTxOutData=state},_),_) -> do
              let rcRate = calcReserveCoinRate bankParam state rate
                  scRate = calcStableCoinRate state rate

                  rates = Rates 
                                  {
                                        pegRate = rate,
                                        scRate = scRate,
                                        rcRate = rcRate
                                  }
                  ratesResponse = RatesResponse{
                    currentCoinsRates = rates
                  }
              logInfo @Prelude.String $ "Current rates: " ++ Prelude.show rates
              tell [toJSON ratesResponse]
          Nothing -> logWarn @Prelude.String $ "Current state is not present yet."
