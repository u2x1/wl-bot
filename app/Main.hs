{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent             ( forkIO )
import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Control.Lens                   ( (^.) )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Core.Data.Unity                ( makeUpdateFromMR'
                                                , makeUpdateFromTG
                                                )
import           Core.Module.Console            ( checkModuleRequirements
                                                , commandProcess
                                                )
import           Core.Type.Mirai.Update        as MR
                                                ( Update )
import           Core.Type.Telegram.Update     as TG
                                                ( Update )
import           Data.Aeson                     ( decode
                                                , eitherDecode
                                                )
import qualified Data.ByteString.Lazy          as BL
import           Network.HTTP.Types             ( status200
                                                , status204
                                                )
import qualified Network.WebSockets            as WS
import           Utils.Config                   ( Config
                                                , mirai_qq_id
                                                , mirai_verify_key
                                                , ws_host
                                                , ws_port
                                                )
import           Utils.Env
import           Utils.Logging                  ( LogTag(Info)
                                                , logErr
                                                , logWT
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , ScottyM
                                                , jsonData
                                                , literal
                                                , post
                                                , status
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
runServer config = do
--  config <- liftIO $ do
--    r <-
--      try $ Wreq.post ((oriConfig ^. mirai_server) <> "verify")
--                      (pairs ("verifyKey" .= (oriConfig ^. mirai_verify_key))) :: IO
--        (Either SomeException (Response BL.ByteString))
--
--    let decodeSessionKey rawHtml = parseMaybe (.: "session") =<< decode rawHtml
--
--    case r of
--      Right r' -> case decodeSessionKey $ r' ^. responseBody of
--        Nothing ->
--          logErr "Parsing request from Mirai" "Failed" >> return oriConfig
--        Just key -> do
--          logWT Info $ "SessionKey: [" <> key <> "]"
--          _ <- Wreq.post ((oriConfig ^. mirai_server) <> "verify") $ pairs
--            ("sessionKey" .= Just key <> "qq" .= (oriConfig ^. mirai_qq_id))
--          return $ set mirai_session_key key oriConfig
--      Left err -> do
--        logErr "Getting session key from Mirai" (show err)
--        return oriConfig

  _ <- checkModuleRequirements
  -- _ <- forkIO (checkModuleEventsIn1Day config)
  -- _ <- forkIO (checkModuleEventsIn5Mins config)

  WS.runClient
    (config ^. ws_host)
    (config ^. ws_port)
    (  "/message?verifyKey="
    <> (config ^. mirai_verify_key)
    <> "&qq="
    <> (config ^. mirai_qq_id)
    )
    (handleQQMsg config)
  --scotty (config ^. port) $ handleTGMsg config

handleTGMsg :: Config -> ScottyM ()
handleTGMsg config = Scotty.post (literal "/telegram/") $ do
  originUpdate <- jsonData :: ActionM TG.Update
  case makeUpdateFromTG originUpdate of
    Just update -> do
      _ <- liftIO $ forkIO (commandProcess update (Env config undefined))
      status status200
    Nothing -> status status204

handleQQMsg :: Config -> WS.Connection -> IO b
handleQQMsg config conn = do
  logWT Info "WebSocket to Mirai connected."
  let env = Env config conn
  forever $ do
    -- m <- WS.receiveData conn :: IO T.Text
    -- logWT Info $ T.unpack m
    msg <- decode <$> WS.receiveData conn :: IO (Maybe MR.Update)
    case msg of
      Just originUpdate -> do
        mrUpdate <- makeUpdateFromMR' originUpdate
        case mrUpdate of
          Just update -> do
            -- logWT Info $ show update
            void $ forkIO (commandProcess update env)
          Nothing -> pure ()
      Nothing -> pure ()
