{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens hiding ((.=))
import           Web.Scotty                as Scotty
import qualified Network.WebSockets        as WS
import           Network.Wreq              as Wreq
import           Network.HTTP.Types                    (status200, status204)
import           Control.Monad                         (forever)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Exception                     (try, SomeException)
import           Control.Concurrent                    (forkIO)
import qualified Data.ByteString.Lazy      as BL
import           Data.Aeson
import           Data.Aeson.Types
import           Core.Data.Unity
import           Core.Type.Telegram.Update as TG
import           Core.Type.Mirai.Update    as MR
--import qualified Data.Text.Lazy as T
import           Core.Plugin.Console
import           Utils.Config
import           Utils.Misc
import           Utils.Logging

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
runServer oriConfig = do
  config <- liftIO $ do
    r <- Wreq.post ((oriConfig ^. mirai_server)<>"auth") $ pairs ("authKey" .= (oriConfig ^. mirai_auth_key))
    let sk = decodeSessionKey $ r ^. responseBody
    logWT Info $ show $ sk
    _ <- Wreq.post ((oriConfig ^. mirai_server)<>"verify") $
      pairs ("sessionKey" .= (sk) <> "qq" .= (oriConfig ^. mirai_qq_id))
    maybe' sk (logErr "Setting Mirai session" "Failed" >> return oriConfig) $ \s ->
      return $ set mirai_session_key s oriConfig

  _ <- checkPluginRequirements
  _ <- liftIO $ forkIO (checkPluginEventsIn1Day config)


  _ <- WS.runClient (config ^. ws_host) (config ^. ws_port) ("/message?sessionKey="<>(config ^. mirai_session_key)) (app config)
  scotty (config ^. port) $ handleTGMsg config

handleTGMsg :: Config -> ScottyM ()
handleTGMsg config =
  Scotty.post (literal "/telegram/") $ do
    originUpdate <- jsonData :: ActionM TG.Update
    case makeUpdateFromTG originUpdate of
      Just update -> do
        _ <- liftIO $ forkIO (commandProcess update config)
        status status200
      Nothing -> status status204

app :: Config -> WS.Connection -> IO b
app config conn = do
    logWT Info "WebSocket connected."
    forever $ do
--        m <- WS.receiveData conn :: IO T.Text
--        logWT Info $ T.unpack m
        msg <- decode <$> WS.receiveData conn :: IO (Maybe MR.Update)
--        logWT Info $ show msg
        case msg of
          Just originUpdate ->
            case makeUpdateFromMR originUpdate of
              Just update -> do
                _ <- logWT Info $ show update
                _ <- liftIO $ forkIO (commandProcess update config)
                liftIO $ pure ()

              Nothing -> liftIO $ pure ()
          Nothing -> liftIO $ pure ()

decodeSessionKey :: BL.ByteString -> Maybe String
decodeSessionKey rawHtml = do
  result <- decode rawHtml
  flip parseMaybe result $ \v -> do
    (v .: "session") >>= return
