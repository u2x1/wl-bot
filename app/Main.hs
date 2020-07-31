{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens hiding ((.=))
import           Web.Scotty                as Scotty
import qualified Network.WebSockets        as WS
import           Network.Wreq              as Wreq
import           Network.HTTP.Types                    (status200, status204)
import           Control.Monad                         (forever, void)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Exception                     (try, SomeException)
import           Control.Concurrent                    (forkIO)
import qualified Data.ByteString.Lazy      as BL
import           Data.Aeson                            (eitherDecode, decode, (.:), pairs, (.=))
import           Data.Aeson.Types                      (parseMaybe)
import           Core.Data.Unity
import           Core.Type.Telegram.Update as TG
import           Core.Type.Mirai.Update    as MR
--import qualified Data.Text.Lazy as T
import           Core.Module.Console
import           Utils.Config
import           Utils.Logging

main :: IO ()
main = do
  cb <- try $ BL.readFile "config.json" :: IO (Either SomeException BL.ByteString)
  case cb of
    Left err -> logErr "Opening config file" (show err)
    Right c ->
      case eitherDecode c :: Either String Config of
        Left err -> logErr "Parsing config file" err
        Right config -> runServer config

runServer :: Config -> IO ()
runServer oriConfig = do
  config <- liftIO $ do
    r <- try $ Wreq.post ((oriConfig ^. mirai_server) <> "auth")
                 (pairs ("authKey" .= (oriConfig ^. mirai_auth_key)))
                   :: IO (Either SomeException (Response BL.ByteString))

    let decodeSessionKey rawHtml = parseMaybe (.: "session") =<< decode rawHtml

    case r of
      Right r' ->
        case decodeSessionKey $ r' ^. responseBody of
          Nothing -> logErr "Parsing request from Mirai" "Failed" >> return oriConfig
          Just key -> do
            logWT Info $ "SessionKey: [" <> key <>"]"
            _ <- Wreq.post ((oriConfig ^. mirai_server) <> "verify") $
              pairs ("sessionKey" .= Just key <>
                     "qq" .= (oriConfig ^. mirai_qq_id))
            return $ set mirai_session_key key oriConfig
      Left err -> do
        logErr "Getting session key from Mirai" (show err)
        return oriConfig

  _ <- checkModuleRequirements
  _ <- forkIO (checkModuleEventsIn1Day config)

  void $ forkIO (WS.runClient (config ^. ws_host)
                              (config ^. ws_port)
                              ("/message?sessionKey=" <> (config ^. mirai_session_key))
                              (handleQQMsg config))
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

handleQQMsg :: Config -> WS.Connection -> IO b
handleQQMsg config conn = do
    logWT Info "WebSocket to Mirai connected."
    forever $ do
--        m <- WS.receiveData conn :: IO T.Text
--        logWT Info $ T.unpack m
        msg <- decode <$> WS.receiveData conn :: IO (Maybe MR.Update)
        case msg of
          Just originUpdate -> do
            mrUpdate <- makeUpdateFromMR' originUpdate
            case mrUpdate of
              Just update ->
--                logWT Info $ show update
                void $ forkIO (commandProcess update config)
              Nothing -> pure ()
          Nothing -> pure ()
