module Status (
    Status
    ) where

import Control.Applicative (pure)
import Data.Aeson (FromJSON(..), ToJSON(..), withText, Value(String))
import qualified Data.Text as DT

data Status = New
    | Incomplete
    | Opinion
    | Invalid
    | WontFix -- Won't Fix
    | Expired
    | Confirmed
    | Triaged
    | InProgress -- In Progress
    | FixCommitted -- Fix Committed
    | FixReleased -- Fix Released
    | Unknown
    deriving (Eq, Read, Show)

instance FromJSON Status where
   parseJSON = withText "Status" $ \t -> pure $ case DT.unpack t of
     "Won't Fix" -> WontFix
     "In Progress" -> InProgress
     "Fix Committed" -> FixCommitted
     "Fix Released" -> FixReleased
     t' -> read t' :: Status

instance ToJSON Status where
   toJSON WontFix = String $ DT.pack "Won't Fix"
   toJSON InProgress = String $ DT.pack "In Progress"
   toJSON FixCommitted = String $ DT.pack "Fix Committed"
   toJSON FixReleased = String $ DT.pack "Fix Released"
   toJSON i = String $ DT.pack $ show i
