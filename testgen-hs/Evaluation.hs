{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Evaluation
  ( WrappedTransactionScriptFailure (..),
    writeJson,
    eval'Conway
  )
where

import Cardano.Ledger.Api (ConwayEra, PParams, TransactionScriptFailure)
import qualified Cardano.Ledger.Api as Ledger
import CLI (GenSize (..), NumCases (..), Seed (..))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy (Proxy)
import qualified Data.Map as Map
import Cardano.Api.Internal.Orphans ()
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QGen
import qualified Test.QuickCheck.Random as QCRandom
import Test.Consensus.Cardano.Generators ()
import Cardano.Ledger.Api.Tx ( PlutusPurpose, RedeemerReport, Tx)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Slot ()
import Cardano.Slotting.Time (SystemStart (..))
import Data.Text (Text)
import Cardano.Ledger.Api.UTxO (UTxO (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (evalTxExUnits)

import Cardano.Ledger.Alonzo.Scripts ( AsIx (..),ExUnits (..),)

import qualified Data.Aeson.Encoding as AesonEncoding
import Encoder(serializeTransactionScriptFailure, ogmiosSuccess)
import Data.List (sortOn)
 
 
newtype WrappedTransactionScriptFailure era = WrappedTransactionScriptFailure
  { unWrappedTransactionScriptFailure ::
      TransactionScriptFailure era
  }

instance ToJSON (WrappedTransactionScriptFailure ConwayEra) where
  toEncoding (WrappedTransactionScriptFailure x) =
    serializeTransactionScriptFailure x
  toJSON wrapped =
    case J.decode (AesonEncoding.encodingToLazyByteString (J.toEncoding wrapped)) :: Maybe J.Value of
      Just v -> v
      Nothing ->
        error "serializeTransactionScriptFailure produced invalid JSON"


instance QC.Arbitrary (Ledger.TransactionScriptFailure Ledger.ConwayEra) where
  arbitrary =
    QC.oneof
      [ Ledger.RedeemerPointsToUnknownScriptHash <$> QC.arbitrary,
        (`Ledger.MissingScript` Map.empty) <$> QC.arbitrary,
        Ledger.MissingDatum <$> QC.arbitrary,
        Ledger.UnknownTxIn <$> QC.arbitrary,
        Ledger.InvalidTxIn <$> QC.arbitrary,
        pure $ Ledger.IncompatibleBudget (ExBudget (ExCPU 999) (ExMemory 888)),
        Ledger.NoCostModelInLedgerState <$> QC.arbitrary,
        Ledger.ContextError <$> QC.arbitrary
      ]

writeJson ::
  Proxy ConwayEra ->
  Seed ->
  GenSize ->
  NumCases ->
  IO ()
writeJson _ (Seed seed) (GenSize size) (NumCases numCases) = do
  let gen :: QGen.Gen [WrappedTransactionScriptFailure ConwayEra]
      gen =
        QC.vectorOf numCases $
          WrappedTransactionScriptFailure <$> (QC.arbitrary :: QC.Gen (TransactionScriptFailure ConwayEra))
      xs :: [WrappedTransactionScriptFailure ConwayEra]
      xs = QGen.unGen gen (QCRandom.mkQCGen seed) size
  BL8.putStrLn (J.encode xs)

eval'Conway ::
  PParams ConwayEra ->
  Tx ConwayEra  ->
  UTxO ConwayEra ->
  EpochInfo (Either Text) ->  
  SystemStart ->
  J.Value
eval'Conway pparams tx utxo epochInfo systemStart =
  case J.decode (AesonEncoding.encodingToLazyByteString (ogmiosSuccess redeemerReport)) of
    Just v -> v
    Nothing -> error "ogmiosSuccess produced invalid JSON"

  where
    redeemerReport :: RedeemerReport ConwayEra
    redeemerReport = selectSingleReport fullReport

    fullReport :: RedeemerReport ConwayEra
    fullReport = evalTxExUnits pparams  tx utxo epochInfo systemStart

    -- Collapse the full report down to a single entry, preferring failures.
    selectSingleReport :: RedeemerReport ConwayEra -> RedeemerReport ConwayEra
    selectSingleReport report =
      case Map.toList failures of
        (purpose, errors) : _ ->
          Map.singleton purpose (Left (pickScriptFailure errors))
        [] ->
          case Map.toList successes of
            (purpose, exUnits) : _ ->
              Map.singleton purpose (Right exUnits)
            [] ->
              error "Empty redeemer report from evalTxExUnits"
      where
        (failures, successes) = Map.foldrWithKey groupReports (Map.empty, Map.empty) report

    groupReports :: Ord (PlutusPurpose AsIx era)
      => PlutusPurpose AsIx era
      -> Either (Ledger.TransactionScriptFailure era) ExUnits
      -> (Map.Map (PlutusPurpose AsIx era) [Ledger.TransactionScriptFailure era ], Map.Map (PlutusPurpose AsIx era) ExUnits)
      -> (Map.Map (PlutusPurpose AsIx era) [Ledger.TransactionScriptFailure era], Map.Map (PlutusPurpose AsIx era) ExUnits)
    groupReports purpose result (failures, successes) =
      case result of
        Left scriptFail -> (Map.unionWith (++) (Map.singleton purpose [scriptFail]) failures, successes)
        Right exUnits -> (failures, Map.singleton purpose exUnits <> successes)

-- | Return the most relevant script failure from a list of errors.
pickScriptFailure ::
  [Ledger.TransactionScriptFailure era]
  -> Ledger.TransactionScriptFailure era
pickScriptFailure xs =
  case sortOn scriptFailurePriority xs of
    [] -> error "Empty list of script failures from the ledger!?"
    x : _ -> x
  where
    scriptFailurePriority ::
      Ledger.TransactionScriptFailure era
      -> Word
    scriptFailurePriority = \case
      Ledger.UnknownTxIn{} -> 0
      Ledger.MissingScript{} -> 0
      Ledger.RedeemerPointsToUnknownScriptHash{} -> 1
      Ledger.NoCostModelInLedgerState{} -> 1
      Ledger.InvalidTxIn{} -> 2
      Ledger.MissingDatum{} -> 3
      Ledger.ContextError{} -> 4
      Ledger.ValidationFailure{} -> 5
      Ledger.IncompatibleBudget{} -> 999
