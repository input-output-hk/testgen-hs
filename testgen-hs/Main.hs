{-# LANGUAGE TypeApplications #-}

module Main where

import CLI (GenSize (..), NumCases (..), Seed (..))
import qualified CLI
import Cardano.Binary (FromCBOR, decodeFull')
import Cardano.Ledger.Api (ConwayEra, PParams)
import qualified Cardano.Ledger.Core
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import qualified Codec.CBOR.Write as C
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Foldable (foldl')
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word16, Word64)
import qualified Deserialize as D
import GHC.Generics (Generic)
import qualified Generators as G
import qualified SynthEvalTx
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.IO as SIO
import qualified System.Random
import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QC
import Test.QuickCheck.Instances.Text ()
import qualified Test.QuickCheck.Random as QC
import qualified Cardano.Ledger.Binary.Decoding as Binary
import qualified Cardano.Ledger.Core as Ledger

main :: IO ()
main =
  CLI.parse >>= \case
    CLI.Generate opts -> runGenerate opts
    CLI.Deserialize cbor -> runDeserialize cbor
    CLI.DeserializeStream -> runDeserializeStream
    CLI.EvaluateStream -> runEvaluateStream

data TestCase a = TestCase
  { cbor :: Text,
    json :: J.Value,
    haskellRepr :: Text,
    typeTag :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Represents initialization payload containing configuration parameters.
--
-- The InitPayload type encapsulates essential system and protocol configuration for eval tx:
--
-- - systemStart: Starting time of the system, CBOR
-- - protocolParams: Protocol parameters in text format, CBOR
-- - slotConfig: Configuration for slots
-- - era: Era identifier as a 16-bit word
--
-- This data type implements Generic, Show and FromJSON type classes for serialization
-- and debugging purposes.
data InitPayload = InitPayload
  { systemStart :: Text, --cbor
    protocolParams :: Text, --cbor
    slotConfig :: SlotConfig,
    era :: Word16
  }
  deriving (Generic, Show, FromJSON)

data SlotConfig = SlotConfig
  { slotLength :: Word64, -- in milliseconds
    zeroSlot :: Word64,
    zeroTime :: Word64,
    epochLength :: Word64
  }
  deriving (Generic, Show, FromJSON)

  -- This is used as a generic response to any incoming request.
data PayloadResponse = PayloadResponse
  { rError :: Maybe Text, 
    rJson :: J.Value
  }
  deriving (Generic, Show)

instance ToJSON PayloadResponse where
  toJSON = J.genericToJSON J.defaultOptions {J.fieldLabelModifier = modifier}
    where
      modifier "rError" = "error"
      modifier "rJson" = "json"
      modifier s = s


-- |
--    EvalPayload represents the data structure for transaction evaluation payload.
--
--    Contains transaction and UTxO information needed for evaluation.
--
--    Fields:
--      * tx - Transaction data as CBOR
--      * utxos - Unspent Transaction Outputs (UTxOs) as CBOR
data EvalPayload = EvalPayload
  { tx :: Text,
    utxos :: Text -- TxIn & TxOut as key value pair CBOR (map)
  }
  deriving (Generic, Show, FromJSON)

runGenerate :: CLI.GenerateOptions -> IO ()
runGenerate (CLI.GenerateOptions maybeSeed genSize numCases command) = do
  SIO.hSetBuffering SIO.stdout SIO.LineBuffering

  seed <- case maybeSeed of
    Just s -> return s
    Nothing -> Seed . round . (* 1000.0) <$> getPOSIXTime

  BL8.putStrLn $ J.encode (J.object [("seed", (\(Seed s) -> J.toJSON s) seed)])

  ( case command of
      CLI.Tx'ConwayDummy -> writeRandom @G.Tx'Conway Proxy
      CLI.ApplyTxErr'Byron -> writeRandom @G.ApplyTxErr'Byron Proxy
      CLI.ApplyTxErr'Shelley -> writeRandom @G.ApplyTxErr'Shelley Proxy
      CLI.ApplyTxErr'Allegra -> writeRandom @G.ApplyTxErr'Allegra Proxy
      CLI.ApplyTxErr'Mary -> writeRandom @G.ApplyTxErr'Mary Proxy
      CLI.ApplyTxErr'Alonzo -> writeRandom @G.ApplyTxErr'Alonzo Proxy
      CLI.ApplyTxErr'Babbage -> writeRandom @G.ApplyTxErr'Babbage Proxy
      CLI.ApplyTxErr'Conway -> writeRandom @G.ApplyTxErr'Conway Proxy
      CLI.DataText -> writeRandom @Text Proxy
      CLI.GHCInteger -> writeRandom @Integer Proxy
      CLI.ExampleADT -> writeRandom @G.ExampleADT Proxy
    )
    seed
    genSize
    numCases

-- | We have to do this in multiple threads, otherwise this generator code is a
--  bottleneck for Rust tests. We still want to deterministically get the same
--  set of test cases for the same seed, but keeping the order is irrelevant,
--  which lets us do less of the costly sync.
writeRandom :: forall a. (Arbitrary a, Show a, G.OurCBOR a) => Proxy a -> Seed -> GenSize -> NumCases -> IO ()
writeRandom _ (Seed seed) (GenSize generatorSize) (NumCases numCases) = do
  let numGreenThreads = 64 -- changing this, changes determinism – how the seed influences the cases
  putsLock <- newMVar ()
  let chunks =
        snd $
          foldl'
            ( \(prevRng, acc) chunk ->
                let (rngL, rngR) = System.Random.split prevRng
                 in (rngL, (chunk, rngR) : acc)
            )
            (QC.mkQCGen seed, [])
            (fairChunks numCases numGreenThreads)
  let worker :: Int -> QC.QCGen -> IO ()
      worker 0 _ = pure ()
      worker n rng1 = do
        let (value :: a, rng2) = splittingUnGen QC.arbitrary rng1 generatorSize
            testCase :: TestCase a = mkTestCase value
        modifyMVar_ putsLock . const . BL8.putStrLn $ J.encode testCase
        worker (n - 1) rng2
  Async.mapConcurrently_ (uncurry worker) chunks

-- | Split `total` into `n` fair chunks, e.g. `chunks 20 3 == [7,7,6]`.
fairChunks :: Int -> Int -> [Int]
fairChunks total n = replicate remainder (base + 1) ++ replicate (n - remainder) base
  where
    (base, remainder) = total `divMod` n

-- | For streaming, we need a version of `unGen` that returns the next RNG –
--  purely and deterministically.
splittingUnGen :: QC.Gen a -> QC.QCGen -> Int -> (a, QC.QCGen)
splittingUnGen (QC.MkGen unGen) rng size =
  let (rng1, rng2) = System.Random.split rng
   in (unGen rng1 size, rng2)

mkTestCase :: forall a. (Show a, G.OurCBOR a) => a -> TestCase a
mkTestCase a =
  TestCase
    { cbor =
        T.decodeUtf8With T.lenientDecode
          . B16.encode
          . BL.toStrict
          . C.toLazyByteString
          . G.ourToCBOR
          $ a,
      haskellRepr = T.pack . show $ a,
      json = G.ourToJSON a,
      typeTag = T.pack . G.unwrappedType $ a
    }

runDeserialize :: ByteString -> IO ()
runDeserialize cbor' =
  case cborToTestCase cbor' of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right a ->
      BL8.putStrLn $
        J.encodePretty'
          (J.defConfig {J.confIndent = J.Spaces 2})
          a

cborToTestCase :: ByteString -> Either String (TestCase a)
cborToTestCase cbor' =
  wrap <$> D.deserialize cbor'
  where
    wrap a =
      let (typeTag', haskellRepr') = G.hfcEnvelopeShowInner a
       in TestCase
            { cbor =
                T.decodeUtf8With T.lenientDecode
                  . B16.encode
                  $ cbor',
              haskellRepr = T.pack haskellRepr',
              json = G.hfcEnvelopeToSubmitApiEnvelope a,
              typeTag = T.pack typeTag'
            }

runDeserializeStream :: IO ()
runDeserializeStream = do
  -- We need line buffering for this to work:
  SIO.hSetBuffering SIO.stdin SIO.LineBuffering
  SIO.hSetBuffering SIO.stdout SIO.LineBuffering
  processLines
  where
    processLines :: IO ()
    processLines = do
      eof <- SIO.isEOF
      if eof
        then pure ()
        else do
          line <- B8.getLine
          let result = case B16.decode line >>= cborToTestCase of
                Right a -> J.toJSON a
                Left err -> J.object ["error" J..= err]
          BL8.putStrLn . J.encode $ result
          processLines

runEvaluateStream :: IO ()
runEvaluateStream = do
  -- We need line buffering for this to work:
  SIO.hSetBuffering SIO.stdin SIO.LineBuffering
  SIO.hSetBuffering SIO.stdout SIO.LineBuffering
  line <- B8.getLine
  case J.eitherDecodeStrict line of
    Left err -> do
      BL8.putStrLn . J.encode $ PayloadResponse
                      { rJson = J.Null,
                        rError = Just . T.pack $ "Expected InitPayload first, but failed to parse line: " ++ err
                      }
      runEvaluateStream
    Right initPayload -> do
      let ei = convertEpochInfo (slotConfig initPayload)
      let combinedResult = do
            pp <- decodeFromHex (protocolParams initPayload)
            ss <- decodeFromHex (systemStart initPayload)
            return (pp, ss)

      case combinedResult of
        Left err -> do
          BL8.putStrLn . J.encode $ PayloadResponse
                      { rJson = J.Null,
                        rError = Just . T.pack $ "Failed to decode initial payload " ++ err
                      }
          exitFailure
        Right (pp, ss) -> do
          BL8.putStrLn . J.encode $ PayloadResponse { rJson = J.object [], rError = Nothing }
          processLines initPayload pp ss ei
  where
    processLines :: InitPayload -> PParams ConwayEra -> SystemStart -> EpochInfo (Either Text) -> IO ()
    processLines initPayload pp ss ei = do
      eof <- SIO.isEOF
      if eof
        then pure ()
        else do
          line <- B8.getLine
          case J.eitherDecodeStrict line of
            Left err -> do
              BL8.putStrLn . J.encode $ PayloadResponse
                      { rJson = J.Null,
                        rError = Just . T.pack $ "Failed to parse line as EvalPayload" ++ err
                      }
              processLines initPayload pp ss ei
            Right (evalPayload :: EvalPayload) -> do
              let evalResult = do
                    let txBytes = either (Left . show) Right $ B16.decode (T.encodeUtf8 (tx evalPayload))
                    decodedTx <- either 
                        Left 
                        (decodeCborWith "Transaction" (Left . show) (Binary.decCBOR @(Cardano.Ledger.Core.Tx ConwayEra)))
                        txBytes
                    utxos <- decodeFromHex (utxos evalPayload)
                    return (decodedTx, utxos)

              case evalResult of
                Left err -> do
                  BL8.putStrLn . J.encode $ PayloadResponse
                      { rJson = J.Null,
                        rError = Just . T.pack $ err
                      }
                  processLines initPayload pp ss ei
                Right (tx, utxos) -> do
                  let result = SynthEvalTx.eval'Conway tx utxos ei ss
                  BL8.putStrLn . J.encode $ PayloadResponse { rJson = result, rError = Nothing }
                  processLines initPayload pp ss ei

-- | Creates an EpochInfo from the given SlotConfig
convertEpochInfo :: SlotConfig -> EpochInfo (Either Text)
convertEpochInfo sc =
  let slotLengthInSeconds :: NominalDiffTime = fromIntegral (slotLength sc) / 1000
   in fixedEpochInfo (EpochSize $ epochLength sc) (mkSlotLength slotLengthInSeconds)

-- | Generic CBOR hex decoder
decodeFromHex :: (FromCBOR a) => T.Text -> Either String a
decodeFromHex hexText = do
  -- 1. Decode from hex
  cborBytes <- first show $ B16.decode (T.encodeUtf8 hexText)
  -- 2. Decode from CBOR.
  first show $ decodeFull' cborBytes


-- Run a CBOR decoder for data in Conway era
decodeCborWith
    :: Text  -- ^ Label for error reporting
    -> (Binary.DecoderError -> Either String a)  -- ^ Error handler
    -> (forall s. Binary.Decoder s a)  -- ^ CBOR decoder
    -> ByteString  -- ^ Input bytes
    -> Either String a
decodeCborWith lbl handleErr decoder bytes =
    case Binary.decodeFullDecoder version lbl decoder (BL.fromStrict bytes) of
        Left err -> handleErr err
        Right val -> Right val
  where
    version = Ledger.eraProtVerLow @ConwayEra