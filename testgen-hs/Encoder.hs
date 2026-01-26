{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Encoder
  ( serializeTransactionScriptFailure
  , serializeDecoderError
  , ogmiosSuccess
  ) where

import qualified Cardano.Ledger.Conway.Scripts      as C
import qualified Cardano.Ledger.Conway.TxInfo       as C

import qualified Cardano.Ledger.Babbage.TxInfo as  Ba


import qualified Cardano.Ledger.Alonzo.Core as Al hiding
    ( TranslationError
    )
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al

import Cardano.Ledger.Api
    ( AsIx (..)
    , PlutusPurpose
    )

import Cardano.Ledger.Plutus
    ( TxOutSource (..)
    )

import qualified Cardano.Ledger.Api             as Ledger
import qualified Cardano.Ledger.TxIn            as Ledger
import qualified Cardano.Ledger.Core            as Ledger
import qualified Cardano.Ledger.Plutus.Language as Ledger
import qualified Cardano.Ledger.BaseTypes  as LedgerBase
import qualified Cardano.Ledger.Binary.Decoding as Binary

import qualified Codec.CBOR.Read as Cbor

import qualified Data.Map as Map

import qualified Cardano.Ledger.Hashes as Hashes
import qualified Cardano.Crypto.Hash.Class   as CC

import qualified Data.Aeson                  as A
import qualified Data.Aeson.Encoding         as J
import           Data.Aeson.Encoding         (Encoding)
import           Data.Foldable               (toList)
import           Data.Int                    (Int64)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE

import qualified Data.ByteString.Base16      as B16

import qualified Data.ByteString.Short       as SBS

import Data.SatInt
    ( fromSatInt
    )
import qualified Data.ByteString as BS

import qualified PlutusLedgerApi.Common as P


import           Prettyprinter               (pretty)

--------------------------------------------------------------------------------
-- Generic helpers
--------------------------------------------------------------------------------

-- | Encode any Foldable as a JSON array using an element encoder.
serializeFoldable
    :: Foldable f
    => (a -> Encoding)
    -> f a
    -> Encoding
serializeFoldable enc =
    J.list enc . toList

--------------------------------------------------------------------------------
-- Hash / TxId / TxIn
--------------------------------------------------------------------------------

-- | Serialize a SafeHash as lower-case hex JSON string.
serializeHash
    :: Hashes.SafeHash crypto
    -> Encoding
serializeHash safeH =
    let CC.UnsafeHash short = Hashes.extractHash safeH
        bs  = SBS.fromShort short
        hex = B16.encode bs
    in J.text (TE.decodeUtf8 hex)

serializDataHash
    :: Al.DataHash
    -> Encoding
serializDataHash =
    serializeHash


serializeTxId
    :: Ledger.TxId
    -> Encoding
serializeTxId txId =
    J.pairs $
      J.pair "id" (serializeHash (Ledger.unTxId txId))

serializeTxIn
    :: Ledger.TxIn
    -> Encoding
serializeTxIn (Ledger.TxIn txId ix) =
    J.pairs $
         J.pair "transaction" (serializeTxId txId)
      <> J.pair "index"       (A.toEncoding ix)

--------------------------------------------------------------------------------
-- Script purpose index
--------------------------------------------------------------------------------

serializeScriptPurposeIndex
    :: forall era. ()
    => C.ConwayPlutusPurpose AsIx era
    -> Encoding
serializeScriptPurposeIndex = \case
    C.ConwaySpending ix ->
        mk ix "spend"

    C.ConwayMinting ix ->
        mk ix "mint"

    C.ConwayCertifying (AsIx ix) ->
        mk ix "publish"

    C.ConwayRewarding ix ->
        mk ix "withdraw"

    C.ConwayVoting (AsIx ix) ->
        mk ix "vote"

    C.ConwayProposing (AsIx ix) ->
        mk ix "propose"
  where
    mk ix purpose =
        J.pairs $
             J.pair "index"   (A.toEncoding ix)
          <> J.pair "purpose" (J.text purpose)

serializeExBudget
    :: P.ExBudget
    -> Encoding
serializeExBudget budget =
    J.pairs $
         J.pair "memory" (J.integer (fromSatInt mem))
      <> J.pair "cpu"    (J.integer (fromSatInt cpu))
  where
    P.ExMemory mem = P.exBudgetMemory budget
    P.ExCPU cpu = P.exBudgetCPU budget


serializeLanguage
    :: Ledger.Language
    -> Encoding
serializeLanguage = \case
    Ledger.PlutusV1 -> "plutus:v1"
    Ledger.PlutusV2 -> "plutus:v2"
    Ledger.PlutusV3 -> "plutus:v3"

-- | Ogmios "budget" object from 'ExUnits'.
serializeExUnits :: Al.ExUnits -> Encoding
serializeExUnits (Al.ExUnits mem cpu) =
      J.pairs $
         J.pair "memory" (J.integer (fromIntegral mem))
      <> J.pair "cpu"    (J.integer (fromIntegral cpu))

txInToText
    :: Ledger.TxIn
    -> Text
txInToText (Ledger.TxIn txid (LedgerBase.TxIx ix)) =
  txIdToText txid <> "#" <> T.pack (show ix)

txIdToText :: Ledger.TxId -> Text
txIdToText (Ledger.TxId (Ledger.originalBytes -> bytes)) =
     TE.decodeUtf8 (B16.encode bytes)

redeemerPtrToText :: forall era. (Ledger.ConwayEraScript era) => PlutusPurpose AsIx era -> Text
redeemerPtrToText = \case
  Ledger.SpendingPurpose (AsIx ix) -> "spend:" <> showT ix
  Ledger.CertifyingPurpose (AsIx ix) -> "publish:" <> showT ix
  Ledger.MintingPurpose (AsIx ix) -> "mint:" <> showT ix
  Ledger.RewardingPurpose (AsIx ix) -> "withdraw:" <> showT ix
  Ledger.ProposingPurpose (AsIx ix) -> "propose:" <> showT ix
  Ledger.VotingPurpose (AsIx ix) -> "vote:" <> showT ix
  _ -> error "unreachable: unknown PlutusPurpose" -- matches _are_ exhaustive, but it’s not provable statically
  where
    showT = T.pack . show -- Word32 → Text



--------------------------------------------------------------------------------
-- Ogmios-style error envelope
--------------------------------------------------------------------------------

ogmiosError
  :: Int64
  -> Text
  -> Encoding       -- ^ payload / "data"
  -> Encoding
ogmiosError code msg payload =
  J.pairs $
       J.pair "code"    (A.toEncoding code)
    <> J.pair "message" (J.text msg)
    <> J.pair "data"    payload

-- | Build the JSON-RPC success envelope that Ogmios returns when every
--   redeemer succeeds.
ogmiosSuccess ::
  Map.Map
    (PlutusPurpose AsIx Ledger.ConwayEra)
    (Either (Ledger.TransactionScriptFailure Ledger.ConwayEra) Al.ExUnits)
  -> J.Encoding
ogmiosSuccess report =
  J.list
    (\(ptr, res) ->
        case res of
          Right exu ->
            J.pairs $
              J.pair "validator" (J.text $ redeemerPtrToText ptr)
              <> J.pair "budget"   (serializeExUnits exu)

          Left err  ->
            J.pairs $
              J.pair "validator" (J.text $ redeemerPtrToText ptr)
              <> J.pair "error"  (
                    J.pairs $
                      J.pair "message" (serializeTransactionScriptFailure err)
                  )
    )
    (Map.toList report)

--makeScriptExecFailure
 --   :: Encoding
  --  -> Encoding
--makeScriptExecFailure = ogmiosError
  --      3010
    --    "Some scripts of the transactions terminated with error(s)."


--------------------------------------------------------------------------------
-- ContextError
--------------------------------------------------------------------------------

serializeContextError
    :: ( PlutusPurpose AsIx era ~ C.ConwayPlutusPurpose AsIx era
       )
    => C.ConwayContextError era
    -> Encoding
serializeContextError err = J.text $ case err of
    C.CertificateNotSupported{} ->
        "A certificate in the transaction isn't supported in neither plutus:v1 nor plutus:v2. Use plutus:v3 or higher."
    C.PlutusPurposeNotSupported{}  ->
        "A script purpose in the transaction isn't supported in neither plutus:v1 nor plutus:v2. Use plutus:v3 or higher."
    C.CurrentTreasuryFieldNotSupported{} ->
        "Unsupported field in transaction: 'treasury'. Use plutus:v3 or higher, or remove this field."
    C.VotingProceduresFieldNotSupported{} ->
        "Unsupported field in transaction: 'votes'. Use plutus:v3 or higher, or remove this field."
    C.ProposalProceduresFieldNotSupported{} ->
        "Unsupported field in transaction: 'proposals'. Use plutus:v3 or higher, or remove this field."
    C.TreasuryDonationFieldNotSupported{} ->
        "Unsupported field in transaction: 'donation'. Use plutus:v3 or higher, or remove this field."
    C.BabbageContextError (Ba.ByronTxOutInContext TxOutFromInput{}) ->
        "Found inputs locked by a (legacy) Byron/Bootstrap address. Don't use those."
    C.BabbageContextError (Ba.ByronTxOutInContext TxOutFromOutput{}) ->
        "Found outputs to a (legacy) Byron/Bootstrap address. Don't use those."
    C.BabbageContextError (Ba.InlineDatumsNotSupported{}) ->
       "Inline datums not supported in plutus:v1. Use plutus:v2 or higher."
    C.BabbageContextError (Ba.ReferenceScriptsNotSupported{}) ->
       "Reference scripts not supported in plutus:v1. Use plutus:v2 or higher."
    C.BabbageContextError (Ba.ReferenceInputsNotSupported{}) ->
       "Reference inputs not supported in plutus:v1. Use plutus:v2 or higher."
    C.BabbageContextError (Ba.RedeemerPointerPointsToNothing purpose) ->
        let (title, ptr) =
                case purpose of
                    C.ConwaySpending (AsIx ix) -> ("spending input", ix)
                    C.ConwayMinting (AsIx ix) -> ("minting policy", ix)
                    C.ConwayCertifying (AsIx ix) -> ("publishing certificate", ix)
                    C.ConwayRewarding (AsIx ix) -> ("withdrawing from account", ix)
                    C.ConwayVoting (AsIx ix) -> ("voting as voter", ix)
                    C.ConwayProposing (AsIx ix) -> ("proposing governance proposal", ix)
          in "Couldn't find corresponding redeemer for " <> title <> " #" <> T.pack (show ptr) <> ". Verify your transaction's construction."
    C.BabbageContextError (Ba.AlonzoContextError (Al.TimeTranslationPastHorizon e)) ->
        "Uncomputable slot arithmetic; transaction's validity bounds go beyond the foreseeable end of the current era: " <> e
    C.BabbageContextError (Ba.AlonzoContextError (Al.TranslationLogicMissingInput i)) ->
        "Unknown transaction input (missing from UTxO set): " <> txInToText i
--------------------------------------------------------------------------------
-- TransactionScriptFailure
--------------------------------------------------------------------------------

serializeTransactionScriptFailure ::
  Ledger.TransactionScriptFailure Ledger.ConwayEra ->
  Encoding
serializeTransactionScriptFailure = \case
  Ledger.RedeemerPointsToUnknownScriptHash reedemers ->
    ogmiosError
      3110
      "Extraneous (non-required) redeemers found in the transaction."
      ( J.pairs $
          J.pair "extraneousRedeemers"
            (serializeFoldable serializeScriptPurposeIndex [reedemers])
      )

  Ledger.MissingScript purpose _ ->
    ogmiosError
      3102
      "An associated script witness is missing."
      ( J.pairs $
          J.pair "missingScripts"
            (serializeFoldable serializeScriptPurposeIndex [purpose])
      )

  Ledger.MissingDatum datum ->
    ogmiosError
      3111
      "Transaction failed because some Plutus scripts are missing their associated datums."
      ( J.pairs $
          J.pair "missingDatums"
            (serializeFoldable serializDataHash [datum])
      )

  Ledger.ValidationFailure _exUnits evalErr traces _ctx ->
    ogmiosError
      3012
      "Some of the scripts failed to evaluate to a positive outcome."
      ( J.pairs $
           J.pair "validationError"
             (J.text (T.pack (show (pretty evalErr))))
        <> J.pair "traces"
             (serializeFoldable A.toEncoding traces)
      )
  Ledger.UnknownTxIn txIn ->
    ogmiosError
      3117
      "The transaction contains unknown UTxO references as inputs."
      (J.pairs $
          J.pair "unknownOutputReferences" (serializeFoldable serializeTxIn [txIn])
      )

  Ledger.InvalidTxIn txIn ->
    ogmiosError
      3013
      "A redeemer points to an input that isn't locked by a Plutus script."
      ( J.pairs $
          J.pair "unsuitableOutputReference" (serializeTxIn txIn)
      )

  Ledger.IncompatibleBudget budget ->
   ogmiosError
      3161
      "The transaction ran out of execution budget!"
      (J.pairs $
          J.pair "budgetUsed" (serializeExBudget budget)
      )

  Ledger.NoCostModelInLedgerState lang ->
   ogmiosError
      3115
      "It seems like the transaction is using a Plutus version for which there's no available cost model yet."
      (J.pairs $
          J.pair "missingCostModels" (serializeFoldable serializeLanguage [lang])
      )

  Ledger.ContextError ctxErr ->
   ogmiosError
      3004
      "Unable to create the evaluation context from the given transaction."
      (J.pairs $
          J.pair "reason" (serializeContextError ctxErr)
      )



-- | Human-readable CBOR decode errors. Kept for Ogmios compatibility & future-use even when unused.
serializeDecoderError :: Int -> Binary.DecoderError -> Text
serializeDecoderError size = extractId . reduceNoise . \case
    Binary.DecoderErrorCanonicityViolation lbl ->
        "couldn't decode due to internal constraint violations on '" <> lbl <> "': \
        \ found CBOR that isn't canonical when I expected it to be."
    Binary.DecoderErrorCustom lbl hint ->
        "couldn't decode due to internal constraint violations on '" <> lbl <> "': " <> hint
    Binary.DecoderErrorDeserialiseFailure lbl (Cbor.DeserialiseFailure offset hint) | offset >= fromIntegral size ->
        "invalid or incomplete value of type '" <> lbl <> "': " <> T.pack (show hint)
    Binary.DecoderErrorDeserialiseFailure lbl (Cbor.DeserialiseFailure offset hint) ->
        "invalid CBOR found at offset [" <> T.pack (show offset) <> "] while decoding a value of type '" <> lbl <> "': "
        <> T.pack (show hint)
    Binary.DecoderErrorEmptyList{} ->
        "couldn't decode due to internal constraint violations on a non-empty list: \
        \must not be empty"
    Binary.DecoderErrorLeftover lbl bytes ->
        "unexpected " <> T.pack (show (BS.length bytes)) <> " bytes found left after \
        \successfully deserialising a/an '" <> lbl <> "'"
    Binary.DecoderErrorSizeMismatch lbl expected actual | expected >= actual ->
        T.pack (show (expected - actual)) <> " missing element(s) in a \
        \data-structure of type '" <> lbl <> "'"
    Binary.DecoderErrorSizeMismatch lbl expected actual ->
        T.pack (show (actual - expected)) <> " extra element(s) in a \
        \data-structure of type '" <> lbl <> "'"
    Binary.DecoderErrorUnknownTag lbl tag ->
        "unknown binary tag (" <> T.pack (show tag) <> ") when decoding a value of type '" <> lbl <> "'\
        \; which is probably because I am trying to decode something else than what \
        \I encountered."
    Binary.DecoderErrorVoid ->
        "impossible: attempted to decode void. Please open an issue."
  where
    extractId = id
    reduceNoise
      = T.replace "\n" " "
      . T.replace "Error: " ""
      . T.replace "Record" "Object / Array"
      . T.replace "Record RecD" "Object / Array"
      . T.replace " ShelleyEra" ""
      . T.replace " AllegraEra" ""
      . T.replace " MaryEra" ""
      . T.replace " AlonzoEra" ""
      . T.replace " BabbageEra" ""
      . T.replace " ConwayEra" ""
      . T.replace "value of type ConwayTxBodyRaw" "transaction body"
      . T.replace "value of type BabbageTxBodyRaw" "transaction body"
      . T.replace "value of type AlonzoTxBodyRaw" "transaction body"
      . T.replace "value of type AllegraTxBodyRaw ()" "transaction body"
      . T.replace "value of type AllegraTxBodyRaw MultiAsset" "transaction body"
      . T.replace "value of type ShelleyTxBodyRaw" "transaction body"
      . T.replace "atbr" ""
      . T.replace "stbr" ""
      . T.replace "btbr" ""
      . T.replace "ctbr" ""
