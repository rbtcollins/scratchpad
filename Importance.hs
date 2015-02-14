module Importance (
    Importance
    ) where

import Control.Applicative (pure)
import Data.Aeson (FromJSON(..), ToJSON(..), withText, Value(String))
import qualified Data.Text as DT

data Importance = Unknown
    | Undecided
    | Critical
    | High
    | Medium
    | Wishlist
    deriving (Eq, Read, Show)

-- TODO: See if there is a magic way to do this, as read/show seem trivial.
instance FromJSON Importance where
   parseJSON = withText "Importance" $ \t -> pure (read (DT.unpack t) :: Importance)
instance ToJSON Importance where
   toJSON i = String $ DT.pack $ show i

