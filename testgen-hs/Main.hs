module Main where

import CLI (GenSize (..), NumCases (..), Seed (..))
import qualified CLI
import qualified Codec.CBOR.Write as C
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
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
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Deserialize as D
import GHC.Generics (Generic)
import qualified Generators as G
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.IO as SIO
import qualified System.Random
import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QC
import Test.QuickCheck.Instances.Text ()
import qualified Test.QuickCheck.Random as QC

main :: IO ()
main =
  CLI.parse >>= \case
    CLI.Generate opts -> runGenerate opts
    CLI.Deserialize cbor -> runDeserialize cbor
    CLI.DeserializeStream -> runDeserializeStream

data TestCase a = TestCase
  { cbor :: Text,
    json :: J.Value,
    haskellRepr :: Text,
    typeTag :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

runGenerate :: CLI.GenerateOptions -> IO ()
runGenerate (CLI.GenerateOptions maybeSeed genSize numCases command) = do
  SIO.hSetBuffering SIO.stdout SIO.LineBuffering

  seed <- case maybeSeed of
    Just s -> return s
    Nothing -> Seed . round . (* 1000.0) <$> getPOSIXTime

  BL8.putStrLn $ J.encode (J.object [("seed", (\(Seed s) -> J.toJSON s) seed)])

  ( case command of
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
