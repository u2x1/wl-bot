{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent             ( forkIO )
import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Control.Lens                   ( (^.)
                                                , set
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Core.Data.Unity                ( makeUpdateFromMR' )
-- import           Web.Scotty                as Scotty
--import qualified Data.Text.Lazy as T
import           Core.Module.Console            ( checkModuleEventsIn1Day
                                                , checkModuleRequirements
                                                , commandProcess
                                                )
import           Core.Type.Mirai.Update        as MR
                                                ( Update )
import           Core.Type.Telegram.Update     as TG
                                                ( )
import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                , decode
                                                , eitherDecode
                                                , pairs
                                                )
import           Data.Aeson.Types               ( parseMaybe )
import qualified Data.ByteString.Lazy          as BL
-- import           Network.HTTP.Types        (status200, status204)
import qualified Network.WebSockets            as WS
import           Network.Wreq                  as Wreq
                                                ( Response
                                                , post
                                                , responseBody
                                                )
import           Utils.Config                   ( Config
                                                , mirai_auth_key
                                                , mirai_qq_id
                                                , mirai_server
                                                , mirai_session_key
                                                , ws_host
                                                , ws_port
                                                )
import           Utils.Logging                  ( LogTag(Info)
                                                , logErr
                                                , logWT
                                                )

main :: IO ()
main = do
  cb <-
    try $ BL.readFile "config.json" :: IO (Either SomeException BL.ByteString)
  case cb of
    Left  err -> logErr "Opening config file" (show err)
    Right c   -> case eitherDecode c :: Either String Config of
      Left  err    -> logErr "Parsing config file" err
      Right config -> runServer config

runServer :: Config -> IO ()
runServer oriConfig = do
  config <- liftIO $ do
    r <-
      try $ Wreq.post ((oriConfig ^. mirai_server) <> "auth")
                      (pairs ("authKey" .= (oriConfig ^. mirai_auth_key))) :: IO
        (Either SomeException (Response BL.ByteString))

    let decodeSessionKey rawHtml = parseMaybe (.: "session") =<< decode rawHtml

    case r of
      Right r' -> case decodeSessionKey $ r' ^. responseBody of
        Nothing ->
          logErr "Parsing request from Mirai" "Failed" >> return oriConfig
        Just key -> do
          logWT Info $ "SessionKey: [" <> key <> "]"
          _ <- Wreq.post ((oriConfig ^. mirai_server) <> "verify") $ pairs
            ("sessionKey" .= Just key <> "qq" .= (oriConfig ^. mirai_qq_id))
          return $ set mirai_session_key key oriConfig
      Left err -> do
        logErr "Getting session key from Mirai" (show err)
        return oriConfig

  _ <- checkModuleRequirements
  _ <- forkIO (checkModuleEventsIn1Day config)
  -- _ <- forkIO (checkModuleEventsIn5Mins config)

  WS.runClient (config ^. ws_host)
               (config ^. ws_port)
               ("/message?sessionKey=" <> (config ^. mirai_session_key))
               (handleQQMsg config)
  -- scotty (config ^. port) $ handleTGMsg config

-- handleTGMsg :: Config -> ScottyM ()
-- handleTGMsg config =
--   Scotty.post (literal "/telegram/") $ do
--     originUpdate <- jsonData :: ActionM TG.Update
--     case makeUpdateFromTG originUpdate of
--       Just update -> do
--         _ <- liftIO $ forkIO (commandProcess update config)
--         status status200
--       Nothing -> status status204

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
