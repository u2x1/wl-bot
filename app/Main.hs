{-# LANGUAGE OverloadedStrings #-}
import           Web.Scotty                as Scotty
import qualified Network.WebSockets        as WS
import           Network.HTTP.Types                    (status500, status200)
import           Control.Monad                         (forever)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Exception                     (try, SomeException)
import           Control.Concurrent                    (forkIO)
import qualified Data.ByteString.Lazy      as BL
import           Data.Aeson
import           Core.Data.Unity
import           Core.Type.Telegram.Update as TG
import           Core.Type.CoolQ.Update    as CQ
import           Core.Plugin.Console
import           Utils.Config
import           Utils.Logging

--import           Network.Wai.Middleware.RequestLogger

app :: Config -> WS.Connection -> IO b
app config conn = do
    logWT Info "WebSocket connected."
    forever $ do
        msg <- decode <$> WS.receiveData conn :: IO (Maybe CQ.Update)
        case msg of
          Just originUpdate ->
            case makeUpdateFromCQ originUpdate of
              Just update -> do
                _ <- liftIO $ forkIO (commandProcess update config)
                liftIO $ pure ()

              Nothing -> liftIO $ pure ()
          Nothing -> liftIO $ pure ()

main :: IO ()
main = do
  cb <- try $ BL.readFile "config.json" :: IO (Either SomeException BL.ByteString)
  case cb of
    Right c ->
      case eitherDecode c :: Either String Config of
        Right config -> runServer config
        Left err -> logErr err "Failed parsing config file"
    Left err -> logErr (show err) "Failed opening config file"

runServer :: Config -> IO ()
runServer config = do
  _ <- checkPluginRequirements
  _ <- liftIO $ forkIO (checkPluginEventsIn1Min config)
  _ <- liftIO $ forkIO (checkPluginEventsIn1Day config)

  _ <- liftIO $ forkIO (WS.runClient (ws_host config) (ws_port config) "/" (app config))
  scotty (port config) $ handleTGMsg config

handleTGMsg :: Config -> ScottyM ()
handleTGMsg config =
  Scotty.post (literal "/telegram/") $ do
    originUpdate <- jsonData :: ActionM TG.Update
    case makeUpdateFromTG originUpdate of
      Just update -> do
        _ <- liftIO $ forkIO (commandProcess update config)
        status status200
      Nothing     -> status status500
