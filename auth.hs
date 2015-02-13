{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}



-- import Data.ByteString.Char8 ()

-- simple
import Control.Concurrent
import Control.Exception (toException, throwIO)
import Control.Monad
import Debug.Trace
import qualified Data.Text as DT
-- import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans
import Data.Maybe
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Exception (SomeException)
import Network.HTTP.Conduit
import System.Random (randomRIO)
-- qualified
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
-- import qualified Data.Text.Lazy          as TL
-- import qualified Data.Text.Lazy.Encoding as LE
import qualified Network.HTTP.Types as W
import qualified Web.Authenticate.OAuth as OA
-- local
import Token
-- local qualified


lpoauth :: OA.OAuth
lpoauth = OA.newOAuth { OA.oauthServerName = "api.launchpad.net"
                      , OA.oauthConsumerKey = "haskell"
                      , OA.oauthConsumerSecret = ""
                      , OA.oauthRequestUri = "https://launchpad.net/+request-token"
                      , OA.oauthSignatureMethod = OA.PLAINTEXT
                      , OA.oauthAuthorizeUri = "https://launchpad.net/+authorize-token"
                      , OA.oauthAccessTokenUri  = "https://launchpad.net/+access-token"
                      }


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Server
    name String
    credentials Token
    UniqueServer name
|]

server_file :: DT.Text
server_file = "servers.sqllite"

safe_show_creds :: OA.Credential -> String
safe_show_creds creds = show $ filter ((`elem` ["oauth_token"]) . fst) $ OA.unCredential creds

safe_show_token :: Token -> String
safe_show_token (Temporary creds) = safe_show_creds creds
safe_show_token (Access creds) = safe_show_creds creds

temp_or_access_token :: IO Token
temp_or_access_token = do
  runSqlite server_file $ do
        maybeServer <- getBy $ UniqueServer "Launchpad"
        case maybeServer of
            Nothing -> do
                temp_creds <- trace "requesting temporary credentials" (withManager $ \manager -> OA.getTemporaryCredential lpoauth manager)
                -- TODO: have something report on pending tokens for users - e.g. in a web UI
                let msg = "New token requested, authorise at " ++ (show $ OA.authorizeUrl lpoauth temp_creds)
                _ <- insert $ trace msg (Server "Launchpad" (Temporary temp_creds))
                return $ Temporary temp_creds
            Just (Entity _ (Server _ temp_creds)) -> trace ("using cached credentials " ++ safe_show_token temp_creds) $ return temp_creds

maybe_token :: IO (Maybe OA.Credential)
maybe_token =  do
  a_token <- liftIO temp_or_access_token
  case a_token of
    -- Try for 30 seconds (interactive testing/watching logfiles - probably
    -- want to discard if in a real app and just drive the state machine)
    Temporary temp_creds -> do
        access_token <- withManager $ \manager -> untilAuthorized manager lpoauth temp_creds 30
        runSqlite server_file $ do
          updateWhere [ServerName ==. "Launchpad"] [ServerCredentials =. (Access access_token)]
        return $ trace ("authorized token " ++ safe_show_creds access_token) (Just access_token)
    Access token -> return (Just token)

main :: IO ()
main = do
    runSqlite server_file $ do
        runMigration migrateAll
    Just access_token <- maybe_token
    req <- liftIO $ parseUrl "https://api.launchpad.net/devel/tripleo/+bug/1302040"
    req2 <- liftIO $ OA.signOAuth lpoauth access_token req
    person_response <- liftIO $ withManager $ \manager -> httpLbs req2 manager
    liftIO $ putStrLn $ show $ responseBody person_response

untilAuthorized :: (MonadIO m, Ord a, Num a)
                => Manager
                -> OA.OAuth
                -> OA.Credential
                -> a
                -> m OA.Credential
untilAuthorized manager lpoauth temp_creds tries = do
    maybe_access_token <- getAccessToken'' id lpoauth temp_creds addAuthBody manager 
    case maybe_access_token of
      Left error_response -> if tries > 0 then do
        liftIO $ threadDelay 1000000 -- Don't spam the server.
        untilAuthorized manager lpoauth temp_creds (tries - 1)
      else
        liftIO . throwIO . OA.OAuthException $ "Gaining OAuth Token Credential Failed: " ++ BSL.unpack (responseBody error_response)
      Right access_token -> do
        return access_token

checkFor401 :: W.Status
           -> W.ResponseHeaders
           -> CookieJar
           -> Maybe SomeException
checkFor401 s@(W.Status sci _) hs cookie_jar =
    if (sci == 401 || sci == 200)
        then Nothing
        else Just $ toException $ StatusCodeException s hs cookie_jar

-- Below is pending the merge of https://github.com/yesodweb/authenticate/pull/42 & release of oauth 1.5.1

getAccessToken'' :: MonadIO m
                => (Request -> Request)  -- ^ Request Hook
                -> OA.OAuth              -- ^ OAuth Application
                -> OA.Credential         -- ^ Temporary Credential (with oauth_verifier if >= 1.0a)
                -> (BS.ByteString -> OA.Credential -> Request -> Request) -- ^ signature style
                -> Manager
                -> m (Either (Response BSL.ByteString) OA.Credential)       -- ^ Token Credential (Access Token & Secret)
getAccessToken'' hook oa cr add_auth manager = do
  let req = hook (fromJust $ parseUrl $ OA.oauthAccessTokenUri oa) { method = "POST" }
  let req2 = req { checkStatus = checkFor401 }
  rsp <- liftIO $ flip httpLbs manager =<< signOAuth' oa (if OA.oauthVersion oa == OA.OAuth10 then OA.delete "oauth_verifier" cr else cr) add_auth req2
  if responseStatus rsp == W.status200
    then do
      let dic = W.parseSimpleQuery . BL.toStrict . responseBody $ rsp
      return $ Right $ OA.Credential dic
    else 
      return $ Left rsp -- liftIO . throwIO . OA.OAuthException $ "Gaining OAuth Token Credential Failed: " ++ BSL.unpack (responseBody rsp)

-- | Add OAuth headers & sign to 'Request'.
signOAuth' :: MonadIO m
          => OA.OAuth              -- ^ OAuth Application
          -> OA.Credential         -- ^ Credential
          -> (BS.ByteString -> OA.Credential -> Request -> Request) -- ^ signature style
          -> Request            -- ^ Original Request
          -> m Request          -- ^ Signed OAuth Request
signOAuth' oa crd add_auth req = do
  crd' <- addTimeStamp =<< addNonce crd
  let tok = injectOAuthToCred oa crd'
  sign <- OA.genSign oa tok req
  return $ add_auth prefix (OA.insert "oauth_signature" sign tok) req
  where
    prefix = case OA.oauthRealm oa of
      Nothing -> "OAuth "
      Just v  -> "OAuth realm=\"" `BS.append` v `BS.append` "\","

-- Note that prefix is used for realm in addAuthHeader, so should fix that up.
addAuthBody :: BS.ByteString -> OA.Credential -> Request -> Request
addAuthBody _ (OA.Credential cred) req =
  urlEncodedBody (filterCreds cred) req

filterCreds :: [(BS.ByteString, BS.ByteString)] -> [(BS.ByteString, BS.ByteString)]
filterCreds cred = filter ((`elem` ["oauth_token", "oauth_verifier", "oauth_consumer_key", "oauth_signature_method", "oauth_timestamp", "oauth_nonce", "oauth_version", "oauth_callback", "oauth_signature"]) . fst) cred


addNonce :: MonadIO m => OA.Credential -> m OA.Credential
addNonce cred = do
  nonce <- liftIO $ replicateM 10 (randomRIO ('a','z')) -- FIXME very inefficient
  return $ OA.insert "oauth_nonce" (BS.pack nonce) cred


addTimeStamp :: MonadIO m => OA.Credential -> m OA.Credential
addTimeStamp cred = do
  stamp <- (floor . (`diffUTCTime` baseTime)) `liftM` liftIO getCurrentTime
  return $ OA.insert "oauth_timestamp" (BS.pack $ show (stamp :: Integer)) cred


injectOAuthToCred :: OA.OAuth -> OA.Credential -> OA.Credential
injectOAuthToCred oa cred =
    OA.inserts [ ("oauth_signature_method", showSigMtd $ OA.oauthSignatureMethod oa)
            , ("oauth_consumer_key", OA.oauthConsumerKey oa)
            , ("oauth_version", "1.0")
            ] cred


showSigMtd :: OA.SignMethod -> BS.ByteString
showSigMtd OA.PLAINTEXT = "PLAINTEXT"
showSigMtd OA.HMACSHA1  = "HMAC-SHA1"
showSigMtd (OA.RSASHA1 _) = "RSA-SHA1"


baseTime :: UTCTime
baseTime = UTCTime day 0
  where
    day = ModifiedJulianDay 40587
