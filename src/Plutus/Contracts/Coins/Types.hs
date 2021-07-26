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


module Plutus.Contracts.Coins.Types
  ( CoinsMachineState (..),
    BankParam (..),
    BankInput (..),
    BankInputAction (..),
    EndpointInput (..),
    RatesResponse (..),
    Rates (..),
    StateResponse (..),
  )
where

import           Data.Aeson                             (FromJSON, ToJSON)
import           GHC.Generics                            (Generic)
import qualified Prelude
import           PlutusTx.Ratio as Ratio
import           Ledger.Scripts               (MonetaryPolicyHash)
import           Ledger.Value                 (TokenName (TokenName))
import           Plutus.Contracts.Oracle.Core
import qualified PlutusTx                      as PlutusTx
import           PlutusTx.Prelude
import           Ledger                        hiding (to)
import           Playground.Contract           (ToSchema)


--State hold by the statemachine
data CoinsMachineState = CoinsMachineState
  { baseReserveAmount :: Integer, -- Current amount of ada reserves held in contract
    stableCoinAmount :: Integer, -- Current amount of stable coins in circulation
    reserveCoinAmount :: Integer, -- Current amount of reserve coins in circulation
    policyScript :: MonetaryPolicyHash -- Policy script used for minting of coins
  }
  deriving stock (Generic,Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--Parameter to parameterized stable coin contract statemachine
data BankParam = BankParam
  { stableCoinTokenName :: TokenName, -- Token name used for stable coin token
    reserveCoinTokenName :: TokenName, -- Token name used for reserve coin token
    minReserveRatio :: Ratio Integer, -- Minimum reserve ratio that must be kept in contract no tokens forging is allowded below the minimum amount 
    maxReserveRatio :: Ratio Integer, -- Maximum reserve ratio that must be kept within contract no tokens forging is allowded above maximum amount
    rcDefaultRate :: Integer, -- Default rate of reserve token if there are no reserve coins minted yet
    oracleParam :: Oracle,    -- Oracle used to getting exchange rate
    oracleAddr :: Address, -- Address of the oracle used to get oracle value to verify its integrity that value is obtained from this oracle address
    bankFee :: Ratio Integer -- Fees charged by contract to contirbute some portion of forged amount to kept in reserve
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

-- Actions that can be performed in stable coin contract
data BankInputAction
  = MintStableCoin Integer
  | RedeemStableCoin Integer
  | MintReserveCoin Integer
  | RedeemReserveCoin Integer
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

type OracleOutput = (TxOutRef, TxOut, Integer) 

-- Redeemer input for updating the state of the contract
data BankInput = BankInput
  { 
    bankInputAction :: BankInputAction, --Action to be performed on contract
    oracleOutput :: OracleOutput -- Oracle output used in the contract to get exchange rate
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

--Data used from the endpoint to be used as input of contract definitions
data EndpointInput = EndpointInput
  { 
    tokenAmount :: Integer -- Tokens amount to be forged
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--Data used for getting current exchange rates of peg, stable coin and reserve coin
data Rates = Rates
  { 
    pegRate :: Integer, -- Current exchange rate of 1 usd to lovelace
    scRate :: Integer, -- Current stable coin exchange rate for 1 stable coin
    rcRate :: Integer -- Curretn reserve coin exchange rate for 1 reserve coin
  }
  deriving stock (Generic,Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--Used as json key to unify response of current rates
data RatesResponse = RatesResponse
  { 
    currentCoinsRates :: Rates
  }
  deriving stock (Generic,Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

--Used as json key to unify response of current state of the statemachine
data StateResponse = StateResponse
  { 
    currentCoinsState :: CoinsMachineState
  }
  deriving stock (Generic,Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)


PlutusTx.makeLift ''CoinsMachineState
PlutusTx.makeLift ''BankParam
PlutusTx.unstableMakeIsData ''CoinsMachineState
PlutusTx.unstableMakeIsData ''BankParam
PlutusTx.unstableMakeIsData ''BankInput
PlutusTx.unstableMakeIsData ''BankInputAction