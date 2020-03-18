{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty                as Scotty
import Network.HTTP.Types                    (status204, status500, status200)
import Network.Wai.Middleware.RequestLogger
import Control.Monad.IO.Class                (liftIO)
import Control.Exception                     (try, SomeException)
import Control.Concurrent                    (forkIO)
import Data.ByteString.Lazy      as BL
import Data.Aeson                            (eitherDecode)
import Core.Data.Unity
import Core.Type.Telegram.Update as TG
import Core.Type.CoolQ.Update    as CQ
import Utils.Config
import Utils.Logging

import Core.Plugin.Console

main :: IO ()
main = do
  cb <- try $ BL.readFile "config.json" :: IO (Either SomeException ByteString)
  case cb of
    Right c ->
      case eitherDecode c :: Either String Config of
        Right config -> runServer config
        Left err -> logErr err "Failed parsing config file"
    Left err -> logErr (show err) "Failed opening config file"

runServer :: Config -> IO ()
runServer config = do
  _ <- liftIO $ forkIO (checkPluginEvents config)
  scotty (port config) $ do
--    middleware logStdoutDev
    handleTGMsg config
    handleCQMsg config

handleTGMsg :: Config -> ScottyM ()
handleTGMsg config =
  Scotty.post (literal "/telegram/") $ do
    originUpdate <- jsonData :: ActionM TG.Update
    case makeUpdateFromTG originUpdate of
      Just update -> do
        _ <- liftIO $ forkIO (commandProcess update config)
        status status204
      Nothing     -> status status200

handleCQMsg :: Config -> ScottyM ()
handleCQMsg config =
  Scotty.post (literal "/cq/") $ do
    originUpdate <- jsonData :: ActionM CQ.Update
    case makeUpdateFromCQ originUpdate of
      Just update -> do
        _ <- liftIO $ forkIO (commandProcess update config)
        status status204
      Nothing -> status status500
