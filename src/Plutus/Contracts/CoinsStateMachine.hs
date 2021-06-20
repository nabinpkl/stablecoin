{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Plutus.Contracts.CoinsStateMachine
  ( scriptInstance,
    machineClient,
    endpoints,
    BankState (..),
    BankParam (..),
    BankInput (..),
    BankInputAction (..),
    BankStateSchema,
    BankStateError,
    EndpointInput (..),
  )
where

import Control.Monad (forever, guard, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger hiding (to)
import qualified Ledger as Interval
import qualified Ledger.Ada as Ada
import Ledger.Constraints (TxConstraints)
import qualified Ledger.Constraints as Constraints
import           Ledger.Scripts               (MonetaryPolicyHash)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import Playground.Contract (ToSchema, adaCurrency, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Plutus.Contract
import Plutus.Contract.StateMachine (SMContractError, State (..), StateMachine, StateMachineClient (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx as PlutusTx
import PlutusTx.Prelude
import PlutusTx.Ratio as Ratio
import qualified Prelude
import Plutus.Contracts.Oracle.Core

--
data BankState = BankState
  { baseReserveAmount :: Integer,
    stableCoinAmount :: Integer,
    reserveCoinAmount :: Integer,
    policyScript :: MonetaryPolicyHash
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--
data BankParam = BankParam
  { stableCoinTokenName :: TokenName,
    reserveCoinTokenName :: TokenName,
    -- oracleAddress :: PubKey,
    minReserveRatio :: Ratio Integer,
    maxReserveRatio :: Ratio Integer,
    rcDefaultRate :: Integer
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--
data BankInputAction
  = MintStableCoin Integer
  | RedeemStableCoin Integer
  | MintReserveCoin Integer
  | RedeemReserveCoin Integer
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

type OracleOutput = (TxOutRef, TxOutTx, Integer)
--
data BankInput = BankInput
  { 
    bankInputAction :: BankInputAction,
    oracleOutput :: OracleOutput
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

{-# INLINEABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINEABLE calcLiablities #-}
calcLiablities :: BankState -> Integer -> Integer
calcLiablities BankState {baseReserveAmount, stableCoinAmount} rate =
  let reserveNeeded = rate * stableCoinAmount
   in min baseReserveAmount reserveNeeded

{-# INLINEABLE calcStableCoinRate #-}
calcStableCoinRate :: BankState -> Integer -> Integer
calcStableCoinRate bs@BankState {stableCoinAmount} rate
  | stableCoinAmount == 0 = rate
  | otherwise = min rate liableRate
  where
    liablities = calcLiablities bs rate
    liableRate = liablities `divide` stableCoinAmount

{-# INLINEABLE calcEquity #-}
calcEquity :: BankState -> Integer -> Integer
calcEquity bs@BankState {baseReserveAmount} rate =
  let liablities = calcLiablities bs rate
   in baseReserveAmount - liablities

{-# INLINEABLE calcReserveCoinRate #-}
calcReserveCoinRate :: BankParam -> BankState -> Integer -> Integer
calcReserveCoinRate BankParam {rcDefaultRate} bs@BankState {reserveCoinAmount} rate
  | reserveCoinAmount /= 0 = rcRate
  | otherwise = rcDefaultRate
  where
    equity = calcEquity bs rate
    rcRate = equity `divide` reserveCoinAmount

--TODO Add external fee for minting and redeeming
--TODO refactor to functions for remove duplicate code
--TODO check for observation slot for oracle so apply date constraints must validate in
--TODO conversion of rc amount and sc amount to base currency
{-# INLINEABLE transition #-}
transition :: BankParam -> State BankState -> BankInput -> Maybe (TxConstraints Void Void, State BankState)
transition bankParam@BankParam {} State {stateData = oldStateData} BankInput {bankInputAction, rate, oracleOutput} = do

  (oref, o, rate) = oracleOutput


  -- let rcRate = calcReserveCoinRate bankParam oldStateData rate
  let scRate = calcStableCoinRate oldStateData rate
      (newConstraints, newStateData) = stateWithConstraints bankInputAction

-- TODO
  guard (isNewStateValid bankParam newStateData rate)

  let state =
        State
          { stateData = newStateData,
            stateValue = Ada.lovelaceValueOf (baseReserveAmount newStateData)
          }

  pure
    ( newConstraints,
      -- <> oracleConstraints,
      -- <>   dateConstraints
      state
    )

{-# INLINEABLE stateWithConstraints #-}
stateWithConstraints :: BankParam -> BankState -> BankInputAction -> Integer -> Integer-> (TxConstraints Void Void, State BankState)
stateWithConstraints bankParam oldStateData bankInputAction scRate rcRate= case bankInputAction of
        MintReserveCoin rcAmt ->
          let constraints = Constraints.mustForgeCurrency (policyScript oldStateData) (reserveCoinTokenName bankParam) rcAmt
              valueInBaseCurrency = rcAmt * rcRate
              newBaseReserve = baseReserveAmount oldStateData + valueInBaseCurrency
           in ( constraints,
                oldStateData
                  { reserveCoinAmount = reserveCoinAmount oldStateData + rcAmt,
                    baseReserveAmount = newBaseReserve
                  }
              )
        RedeemReserveCoin rcAmt ->
          let constraints = Constraints.mustForgeCurrency (policyScript oldStateData) (reserveCoinTokenName bankParam) (negate rcAmt)
              valueInBaseCurrency = rcAmt * rcRate
              newBaseReserve = baseReserveAmount oldStateData - valueInBaseCurrency
           in ( constraints,
                oldStateData
                  { reserveCoinAmount = reserveCoinAmount oldStateData - rcAmt,
                    baseReserveAmount = newBaseReserve
                  }
              )
        MintStableCoin scAmt ->
          let constraints = Constraints.mustForgeCurrency (policyScript oldStateData) (stableCoinTokenName bankParam) scAmt
              valueInBaseCurrency = scAmt * scRate
              newBaseReserve = baseReserveAmount oldStateData + valueInBaseCurrency
           in ( constraints,
                oldStateData
                  { stableCoinAmount = stableCoinAmount oldStateData + scAmt,
                    baseReserveAmount = newBaseReserve
                  }
              )
        RedeemStableCoin scAmt ->
          let constraints = Constraints.mustForgeCurrency (policyScript oldStateData) (stableCoinTokenName bankParam) (negate scAmt)
              valueInBaseCurrency = scAmt * scRate
              newBaseReserve = baseReserveAmount oldStateData - valueInBaseCurrency
           in ( constraints,
                oldStateData
                  { stableCoinAmount = stableCoinAmount oldStateData - scAmt,
                    baseReserveAmount = newBaseReserve
                  }
              )

{-# INLINEABLE isNewStateValid #-}
isNewStateValid :: BankParam -> BankState -> Integer -> Bool
isNewStateValid bankParam bankState rate = isRight (checkForValidState bankParam bankState rate)

{-# INLINEABLE checkForValidState #-}
checkForValidState :: BankParam -> BankState -> Integer -> Either ErrorState ()
checkForValidState bankParam bankState@BankState {baseReserveAmount, stableCoinAmount, reserveCoinAmount} rate = do
  unless (baseReserveAmount >= 0) (Left NegativeReserves)
  unless (reserveCoinAmount >= 0) (Left NegativeReserveCoins)
  unless (stableCoinAmount >= 0) (Left NegativeStablecoins)
  unless (calcLiablities bankState rate >= 0) (Left NegativeLiabilities)
  unless (calcEquity bankState rate >= 0) (Left NegativeEquity)

  let actualReserves = fromInteger baseReserveAmount
      allowedReserves = (,) <$> minReserve bankParam bankState rate <*> maxReserve bankParam bankState rate

  case allowedReserves of
    Just (minReserves, maxReserves) -> do
      unless (actualReserves >= minReserves) (Left $ MinReserves minReserves actualReserves)
      unless (actualReserves <= maxReserves) (Left $ MaxReserves maxReserves actualReserves)
    Nothing -> pure ()

{-# INLINEABLE minReserve #-}
minReserve :: BankParam -> BankState -> Integer -> Maybe (Ratio Integer)
minReserve BankParam {minReserveRatio} BankState {stableCoinAmount} rate
  | stableCoinAmount == 0 = Nothing
  | otherwise =
    let currentScValue = rate * stableCoinAmount
     in Just $ minReserveRatio * (fromInteger currentScValue)

{-# INLINEABLE maxReserve #-}
maxReserve :: BankParam -> BankState -> Integer -> Maybe (Ratio Integer)
maxReserve BankParam {maxReserveRatio} BankState {stableCoinAmount} rate
  | stableCoinAmount == 0 = Nothing
  | otherwise =
    let currentScValue = rate * stableCoinAmount
     in Just $ maxReserveRatio * (fromInteger currentScValue)

data ErrorState
  = NegativeReserveCoins
  | NegativeReserves
  | NegativeStablecoins
  | MinReserves {allowed :: Ratio Integer, actual :: Ratio Integer}
  | MaxReserves {allowed :: Ratio Integer, actual :: Ratio Integer}
  | NegativeLiabilities
  | NegativeEquity
  deriving (Prelude.Show)

bankMachine :: BankParam -> StateMachine BankState BankInput
bankMachine bankParam = SM.mkStateMachine Nothing (transition bankParam) isFinal
  where
    isFinal _ = False

scriptInstance :: BankParam -> Scripts.TypedValidator (StateMachine BankState BankInput)
scriptInstance bankParam =
  let val = $$(PlutusTx.compile [||validator||]) `PlutusTx.applyCode` PlutusTx.liftCode bankParam
      validator param = SM.mkValidator (bankMachine param)
      wrap = Scripts.wrapValidator @BankState @BankInput
   in Scripts.mkTypedValidator @(StateMachine BankState BankInput) val $$(PlutusTx.compile [||wrap||])

machineClient ::
  Scripts.TypedValidator (StateMachine BankState BankInput) ->
  BankParam ->
  StateMachineClient BankState BankInput
machineClient scriptInst bankParam =
  let machine = bankMachine bankParam
   in SM.mkStateMachineClient (SM.StateMachineInstance machine scriptInst)

initialState :: StateMachineClient BankState BankInput -> BankState
initialState StateMachineClient {scInstance = SM.StateMachineInstance {SM.typedValidator}} =
  BankState
    { baseReserveAmount = 0,
      stableCoinAmount = 0,
      reserveCoinAmount = 0,
      policyScript = Scripts.forwardingMonetaryPolicyHash typedValidator
    }

data BankStateError
  = StartError ContractError
  | StateMachineError SMContractError
  | RunStepError ContractError
  deriving stock (Prelude.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

stableCoinName :: TokenName
stableCoinName = "StableToken"

reserveCoinName :: TokenName
reserveCoinName = "ReserveToken"

bp :: BankParam
bp =
  BankParam
    { stableCoinTokenName = stableCoinName,
      reserveCoinTokenName = reserveCoinName,
      minReserveRatio = zero,
      maxReserveRatio = 4 % 1,
      rcDefaultRate = 1000000
    }

client :: StateMachineClient BankState BankInput
client = machineClient (scriptInstance bp) bp

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . Prelude.show

start :: HasBlockchainActions s => Integer -> Contract w s Text ()
start _ = do
  void $ mapError' $ SM.runInitialise client (initialState client) mempty

--TODO check for validation in offchain
mintStableCoin :: HasBlockchainActions s => Oracle -> EndpointInput -> Contract w s Text ()
mintStableCoin oracleParam endpointInput@EndpointInput {tokenAmount} = do
  logInfo @Prelude.String $ Prelude.show oracleParam
  oracle <- findOracle oracleParam
  case oracle of
    Nothing -> logInfo @Prelude.String "Oracle not found"
    Just (oref, o, x) -> do
      logInfo @Prelude.String $ Prelude.show oref
      logInfo @Prelude.String $ Prelude.show o
      logInfo @Prelude.String $ Prelude.show x
      let v = txOutValue (txOutTxOut o) <> Ada.lovelaceValueOf (oFee oracleParam)
      logInfo @Prelude.String $ Prelude.show v

      let input =
        BankInput
          { rate = getRatioFromInput endpointInput,
            bankInputAction = MintStableCoin tokenAmount,
            oracleOutput = (oref, o, x)
          }
      void $ mapError' $ SM.runStep client input

redeemStableCoin :: HasBlockchainActions s => EndpointInput -> Contract w s Text ()
redeemStableCoin endpointInput@EndpointInput {tokenAmount} = do
  let input =
        BankInput
          { rate = getRatioFromInput endpointInput,
            bankInputAction = RedeemStableCoin tokenAmount
          }
  void $ mapError' $ SM.runStep client input

mintReserveCoin :: HasBlockchainActions s => EndpointInput -> Contract w s Text ()
mintReserveCoin endpointInput@EndpointInput {tokenAmount} = do
  let input =
        BankInput
          { rate = getRatioFromInput endpointInput,
            bankInputAction = MintReserveCoin tokenAmount
          }
  void $ mapError' $ SM.runStep client input

redeemReserveCoin :: HasBlockchainActions s => EndpointInput -> Contract w s Text ()
redeemReserveCoin endpointInput@EndpointInput {tokenAmount} = do
  let input =
        BankInput
          { rate = getRatioFromInput endpointInput,
            bankInputAction = RedeemReserveCoin tokenAmount
          }
  void $ mapError' $ SM.runStep client input

getRatioFromInput :: EndpointInput -> Ratio Integer
getRatioFromInput EndpointInput {rateNume, rateDeno} = rateNume % rateDeno

data EndpointInput = EndpointInput
  { --Signed message from oracle provider  for exchange rate
    rateNume :: Integer,
    rateDeno :: Integer,
    tokenAmount :: Integer
  }
  deriving stock (Generic, Prelude.Eq, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type BankStateSchema =
  BlockchainActions
    .\/ Endpoint "start" Integer
    .\/ Endpoint "mintStableCoin" EndpointInput
    .\/ Endpoint "redeemStableCoin" EndpointInput
    .\/ Endpoint "mintReserveCoin" EndpointInput
    .\/ Endpoint "redeemReserveCoin" EndpointInput

mkSchemaDefinitions ''BankStateSchema

endpoints :: Oracle -> Contract () BankStateSchema Text ()
endpoints oracle =
  ( start'
      `select` mintStableCoin'
      `select` redeemStableCoin'
      `select` mintReserveCoin'
      `select` redeemReserveCoin'
  )
    >> endpoints oracle
  where
    --TODO handle state for multiple start endpoint call
    start' = endpoint @"start" >>= start
    mintStableCoin' = endpoint @"mintStableCoin" >>= mintStableCoin oracle
    redeemStableCoin' = endpoint @"redeemStableCoin" >>= redeemStableCoin
    mintReserveCoin' = endpoint @"mintReserveCoin" >>= mintReserveCoin
    redeemReserveCoin' = endpoint @"redeemReserveCoin" >>= redeemReserveCoin

PlutusTx.makeLift ''BankState
PlutusTx.makeLift ''BankParam
PlutusTx.unstableMakeIsData ''BankState
PlutusTx.unstableMakeIsData ''BankParam
PlutusTx.unstableMakeIsData ''BankInput
PlutusTx.unstableMakeIsData ''BankInputAction