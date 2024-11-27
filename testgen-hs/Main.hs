module Main where

import CLI (GenSize (..), NumCases (..), Seed (..))
import qualified CLI
import qualified Codec.CBOR.Write as C
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
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
import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QC (unGen)
import Test.QuickCheck.Instances.Text ()
import qualified Test.QuickCheck.Random as QC (mkQCGen)

main :: IO ()
main =
  CLI.parse >>= \case
    CLI.Generate opts -> runGenerate opts
    CLI.Deserialize cbor -> runDeserialize cbor
    CLI.DeserializeStream -> runDeserializeStream

data Output a = Output
  { seed :: Int,
    testCases :: [TestCase a]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data TestCase a = TestCase
  { cbor :: Text,
    json :: J.Value,
    haskellRepr :: Text,
    typeTag :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

runGenerate :: CLI.GenerateOptions -> IO ()
runGenerate (CLI.GenerateOptions maybeSeed genSize numCases command) = do
  seed <- case maybeSeed of
    Just s -> return s
    Nothing -> Seed . round . (* 1000.0) <$> getPOSIXTime

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

writeRandom :: forall a. (Arbitrary a, Show a, G.OurCBOR a) => Proxy a -> Seed -> GenSize -> NumCases -> IO ()
writeRandom _ (Seed seed) (GenSize generatorSize) (NumCases numCases) = do
  let qcGen = QC.mkQCGen seed
      values :: [a] = QC.unGen (QC.vectorOf numCases QC.arbitrary) qcGen generatorSize
      testCases :: [TestCase a] = mkTestCase <$> values
      output = Output {seed, testCases}
  BL8.putStrLn $ J.encodePretty' (J.defConfig {J.confIndent = J.Spaces 2}) output

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
