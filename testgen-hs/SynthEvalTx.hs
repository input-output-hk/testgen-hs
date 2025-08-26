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
import Cardano.Ledger.Alonzo.Scripts
  ( AsIx (..),
    ExUnits (..),
  )
import qualified Cardano.Ledger.Alonzo.Scripts
import qualified Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Api (EraTx, PParams, bodyTxL, collateralInputsTxBodyL, inputsTxBodyL, referenceInputsTxBodyL)
import qualified Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Scripts
  ( ConwayEraScript,
    pattern CertifyingPurpose,
    pattern MintingPurpose,
    pattern ProposingPurpose,
    pattern RewardingPurpose,
    pattern SpendingPurpose,
    pattern VotingPurpose,
  )
import Cardano.Ledger.Api.Tx (BabbageEraTxBody, PlutusPurpose, RedeemerReport, Tx)
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
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Aeson
  ( eitherDecodeStrict',
  )
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.Default
import Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Lens.Micro ((^.))
import qualified Test.Cardano.Ledger.Generic.GenState
import qualified Test.Cardano.Ledger.Generic.Proof as Proof
import qualified Test.Cardano.Ledger.Generic.TxGen
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
  slot <- SlotNo <$> QC.choose (0, 10_000)
  ((utxo, tx), _) <- Test.Cardano.Ledger.Generic.GenState.runGenRS proof Data.Default.def (Test.Cardano.Ledger.Generic.TxGen.genAlonzoTx proof slot)
  pure (tx, utxo)
  where
    proof :: Proof.Proof Cardano.Ledger.Api.Era.ConwayEra
    proof = Proof.Conway

eval'Conway ::
  (Cardano.Ledger.Core.Tx (Cardano.Ledger.Api.Era.ConwayEra))  ->
  UTxO (Cardano.Ledger.Api.Era.ConwayEra) ->
  EpochInfo (Either Text) ->
  SystemStart ->
  J.Value
eval'Conway tx utxo epochInfo systemStart = ogmiosSuccess redeemerReport
  where
    redeemerReport :: RedeemerReport (Cardano.Ledger.Api.Era.ConwayEra)
    redeemerReport = evalTxExUnits protocolParams tx utxo epochInfo systemStart

-- | Version of eval'Conway that uses dummy epoch info and system start
eval'ConwayDummy :: (Cardano.Ledger.Core.Tx (Cardano.Ledger.Api.Era.ConwayEra)) -> UTxO Cardano.Ledger.Api.Era.ConwayEra -> J.Value
eval'ConwayDummy tx utxo = ogmiosSuccess redeemerReport
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

-- | Render a ledger redeemer pointer the Ogmios way
redeemerPtrToText :: forall era. (ConwayEraScript era) => PlutusPurpose AsIx era -> Text
redeemerPtrToText = \case
  SpendingPurpose (AsIx ix) -> "spend:" <> showT ix
  CertifyingPurpose (AsIx ix) -> "publish:" <> showT ix
  MintingPurpose (AsIx ix) -> "mint:" <> showT ix
  RewardingPurpose (AsIx ix) -> "withdraw:" <> showT ix
  ProposingPurpose (AsIx ix) -> "propose:" <> showT ix
  VotingPurpose (AsIx ix) -> "vote:" <> showT ix
  _ -> error "unreachable: unknown PlutusPurpose" -- matches _are_ exhaustive, but it’s not provable statically
  where
    showT = T.pack . show -- Word32 → Text

-- | Ogmios "budget" object from 'ExUnits'.
exUnitsToJSON :: ExUnits -> J.Value
exUnitsToJSON (ExUnits mem cpu) =
  J.object ["memory" J..= mem, "cpu" J..= cpu]

-- | Build the JSON-RPC success envelope that Ogmios returns when every
--   redeemer succeeds.
ogmiosSuccess ::
  forall era err.
  (ConwayEraScript era, Show err) => -- same constraint here
  Map.Map (PlutusPurpose AsIx era) (Either err ExUnits) ->
  J.Value
ogmiosSuccess report =
  J.toJSON
    [ case res of
        Right exu ->
          J.object
            [ "validator" J..= redeemerPtrToText ptr,
              "budget" J..= exUnitsToJSON exu
            ]
        Left err ->
          J.object
            [ "validator" J..= redeemerPtrToText ptr,
              "error"
                J..= J.object
                  ["message" J..= show err] -- simple text trace
            ]
      | (ptr, res) <- Map.toList report
    ]