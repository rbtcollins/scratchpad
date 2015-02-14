{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}


import Control.Applicative(pure)
import Control.Monad.Trans (liftIO)
import Database.Persist.Sqlite
import Data.Aeson (decode, encode, FromJSON(..), ToJSON(..), withText, Value(String))
import Data.Time (ZonedTime)
import Data.Aeson.TH (deriveJSON, defaultOptions)
-- import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as DT
import qualified Network.HTTP.Conduit as Conduit
import qualified Web.Authenticate.OAuth as OA

import qualified Auth
import Importance (Importance)
import Status (Status)

-- This possibly/probably doesn't want to live here.
serverFile :: DT.Text
serverFile = "servers.sqllite"

-- Example JSON for a bug. Reference is at https://api.launchpad.net/devel/
-- "{\"date_closed\": null
--  \"assignee_link\": null
--  \"bug_link\": \"https://api.launchpad.net/devel/bugs/1302040\"
--  \"bug_target_display_name\": \"tripleo\"
--  \"bug_target_name\": \"tripleo\"
--  \"bug_watch_link\": null
--  \"date_assigned\": null
--  \"date_confirmed\": \"2014-04-08T18:59:02.536753+00:00\"
--  \"date_created\": \"2014-04-03T15:58:03.572018+00:00\"
--  \"date_fix_committed\": null
--  \"date_fix_released\": null
--  \"date_in_progress\": null
--  \"date_incomplete\": null
--  \"date_left_closed\": null
--  \"date_left_new\": \"2014-04-08T18:59:02.536753+00:00\"
--  \"date_triaged\": \"2014-04-08T18:59:02.536753+00:00\"
--  \"http_etag\": \"\\\"d26c2c966337183b80c92c17ac1e60f258c93b6a-574aa9e827a31e080fae23652ebb4f12e1ee50bf\\\"\"
--  \"importance\": \"High\"
--  \"is_complete\": false}"
--  \"milestone_link\": null
--  \"owner_link\": \"https://api.launchpad.net/devel/~haneef\"
--  \"related_tasks_collection_link\": \"https://api.launchpad.net/devel/tripleo/+bug/1302040/related_tasks\"
--  \"resource_type_link\": \"https://api.launchpad.net/devel/#bug_task\"
--  \"self_link\": \"https://api.launchpad.net/devel/tripleo/+bug/1302040\"
--  \"status\": \"Triaged\"
--  \"target_link\": \"https://api.launchpad.net/devel/tripleo\"
--  \"title\": \"Bug #1302040 in tripleo: \\\"Admin role name should never be hardcoded\\\"\"
--  \"web_link\": \"https://bugs.launchpad.net/tripleo/+bug/1302040\"


data LaunchpadBugTask = LaunchpadBugTask { 
    date_assigned :: Maybe ZonedTime
    , date_closed :: Maybe ZonedTime
    , date_confirmed :: Maybe ZonedTime
    , date_created :: Maybe ZonedTime
    , date_fix_committed :: Maybe ZonedTime
    , date_fix_released :: Maybe ZonedTime
    , date_in_progress :: Maybe ZonedTime
    , date_incomplete :: Maybe ZonedTime
    , date_left_closed :: Maybe ZonedTime
    , date_left_new :: Maybe ZonedTime
    , date_triaged :: Maybe ZonedTime
    , importance :: Importance
    , is_complete :: Bool
    , status :: Status
    , title :: String
    }
    deriving (Show)

$(deriveJSON defaultOptions ''LaunchpadBugTask)


main :: IO ()
main = do
    runSqlite serverFile $ runMigration Auth.migrateAll
    Just access_token <- Auth.maybeToken
    req <- liftIO $ Conduit.parseUrl "https://api.launchpad.net/devel/tripleo/+bug/1302040"
    req2 <- liftIO $ OA.signOAuth Auth.lpoauth access_token req
    bug_bytes <- liftIO $ Conduit.withManager $ \manager -> Conduit.httpLbs req2 manager
    bug <- return (decode (Conduit.responseBody bug_bytes) :: Maybe LaunchpadBugTask)
    print $ Conduit.responseBody bug_bytes
    print bug
