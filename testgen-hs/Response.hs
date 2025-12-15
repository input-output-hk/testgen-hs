module Response
  ( PayloadResponse (..),
    OgmiosError (..)
  ) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Generic response envelope used across endpoints.
data PayloadResponse = PayloadResponse
  { rError :: Maybe Text,
    rJson :: Maybe J.Value
  }
  deriving (Generic, Show)

instance ToJSON PayloadResponse where
  toJSON =
    J.genericToJSON
      J.defaultOptions
        { J.fieldLabelModifier = modifier,
          J.omitNothingFields = True
        }
    where
      modifier "rError" = "error"
      modifier "rJson" = "json"
      modifier s = s

data OgmiosError = OgmiosError
  { code :: Int64,
    message :: Text,
    value :: J.Value
  }
  deriving (Generic, Show)

instance ToJSON OgmiosError where
  toJSON =
    J.genericToJSON
      J.defaultOptions
        { J.fieldLabelModifier = \case
            "value" -> "data"
            s -> s
        }

