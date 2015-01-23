import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as LE
import           Network.HTTP.Conduit

main :: IO ()
main = do
    input <- getContents
    let username = head $ words input
    let password = head $ tail $ words input
    request <- parseUrl "https://bugs.launchpad.net/"
    text <- withManager $ \manager -> do
        response <- httpLbs request manager
        return . LE.decodeUtf8 $ responseBody response
    putStr $ TL.unpack $ text

