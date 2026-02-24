{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Generates a fake minimal UTxO set for a given transaction, for the purposes
--  of running `Cardano.Ledger.Alonzo.Plutus.Evaluate.evalTxExUnits` on the
--  transaction.
module SynthEvalTx (eval'Conway, eval'ConwayDummy, genTxUTxO, stubUTxO) where

import Cardano.Crypto.Hash.Class (hashFromBytes)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (evalTxExUnits)
import qualified Cardano.Ledger.Alonzo.Scripts
import qualified Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Api (EraTx, PParams, bodyTxL, collateralInputsTxBodyL, inputsTxBodyL, referenceInputsTxBodyL)
import qualified Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Tx (BabbageEraTxBody, RedeemerReport, Tx)
import Cardano.Ledger.Api.Tx.In (TxIn)
import Cardano.Ledger.Api.Tx.Out
  ( TxOut (..),
  )
import Cardano.Ledger.Api.UTxO (UTxO (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core
import Cardano.Ledger.Credential
  ( Credential (KeyHashObj),
    StakeReference (StakeRefNull),
  )
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Val (inject)
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Aeson
  ( eitherDecodeStrict',
  )
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as AesonEncoding
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Encoder (ogmiosSuccess)
import Lens.Micro ((^.))
import qualified Test.Cardano.Ledger.Generic.GenState
import qualified Test.Cardano.Ledger.Generic.Proof as Proof
import qualified Test.QuickCheck as QC

-- | Drops a Plutus script into both lookup maps.
_registerPlutus ::
  forall era.
  (Proof.Reflect era) =>
  Test.Cardano.Ledger.Generic.GenState.PlutusPurposeTag ->
  Bool ->
  Cardano.Ledger.Alonzo.Scripts.Script era ->
  Test.Cardano.Ledger.Generic.GenState.GenRS era ()
_registerPlutus tag ok scr = do
  let h = Cardano.Ledger.Core.hashScript @era scr
  Test.Cardano.Ledger.Generic.GenState.modifyPlutusScripts (Map.insert (h, tag) (Cardano.Ledger.Alonzo.Tx.IsValid ok, scr))
  Test.Cardano.Ledger.Generic.GenState.modifyGenStateScripts (Map.insert h scr)

genTxUTxO ::
  QC.Gen
    ( Cardano.Ledger.Core.Tx Cardano.Ledger.Api.Era.ConwayEra,
      Cardano.Ledger.Api.UTxO.UTxO Cardano.Ledger.Api.Era.ConwayEra
    )
genTxUTxO = do
  tx <- QC.arbitrary
  pure (tx, stubUTxO tx)

eval'Conway ::
  (Cardano.Ledger.Core.Tx (Cardano.Ledger.Api.Era.ConwayEra)) ->
  UTxO (Cardano.Ledger.Api.Era.ConwayEra) ->
  EpochInfo (Either Text) ->
  SystemStart ->
  J.Value
eval'Conway tx utxo epochInfo systemStart =
  case J.decode (AesonEncoding.encodingToLazyByteString (ogmiosSuccess redeemerReport)) of
    Just v -> v
    Nothing -> error "ogmiosSuccess produced invalid JSON"
  where
    redeemerReport :: RedeemerReport (Cardano.Ledger.Api.Era.ConwayEra)
    redeemerReport = evalTxExUnits protocolParams tx utxo epochInfo systemStart

-- | Version of eval'Conway that uses dummy epoch info and system start
eval'ConwayDummy :: (Cardano.Ledger.Core.Tx (Cardano.Ledger.Api.Era.ConwayEra)) -> UTxO Cardano.Ledger.Api.Era.ConwayEra -> J.Value
eval'ConwayDummy tx utxo =
  case J.decode (AesonEncoding.encodingToLazyByteString (ogmiosSuccess redeemerReport)) of
    Just v -> v
    Nothing -> error "ogmiosSuccess produced invalid JSON"
  where
    redeemerReport :: RedeemerReport (Cardano.Ledger.Api.Era.ConwayEra)
    redeemerReport = evalTxExUnits protocolParams tx utxo dummyEpochInfo dummySystemStart

-- | Collect every input the Tx spends, in any role.
allTxIns :: (BabbageEraTxBody era, EraTx era) => Tx era -> Set.Set TxIn
allTxIns tx =
  b ^. inputsTxBodyL
    <> b ^. collateralInputsTxBodyL
    <> b ^. referenceInputsTxBodyL
  where
    b = tx ^. bodyTxL

-- | Builds a dummy TxOut for each of the inputs.
stubUTxO :: forall era. (BabbageEraTxBody era, EraTx era) => Tx era -> UTxO era
stubUTxO tx =
  UTxO . Map.fromList $
    [(i, dummyOut) | i <- Set.toList (allTxIns tx)]
  where
    dummyOut :: TxOut era
    dummyOut = mkBasicTxOut dummyAddr (inject (Coin 2_000_000))

    dummyAddr :: Addr
    dummyAddr = Addr Testnet (KeyHashObj dummyKeyHash) StakeRefNull

    dummyKeyHash :: KeyHash 'Payment
    dummyKeyHash =
      case hashFromBytes (BS.replicate 28 0) of
        Just h -> KeyHash h
        Nothing -> error "hashFromBytes: unexpected length mismatch"

dummyEpochInfo :: EpochInfo (Either Text)
dummyEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

dummySystemStart :: SystemStart
dummySystemStart =
  SystemStart $ UTCTime (fromGregorian 2020 7 29) (secondsToDiffTime 0)

protocolParamsJSON :: BS.ByteString
protocolParamsJSON = $(embedFile "protocol-params-preview.json")

protocolParams :: PParams (Cardano.Ledger.Api.Era.ConwayEra)
protocolParams =
  case eitherDecodeStrict' protocolParamsJSON of
    Left err -> error $ "Embedded protocol-parameters JSON is malformed:\n" <> err
    Right pp -> pp
