{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty                as Scotty
import Network.HTTP.Types                    (status204)
-- import Network.Wai.Middleware.RequestLogger
import Control.Monad.IO.Class                (liftIO)
import Control.Exception                     (try, SomeException)
import Control.Concurrent                    (forkFinally, forkIO)
import Data.ByteString.Lazy      as BL
import Data.Aeson                            (eitherDecode)
import Core.Type.Telegram.Update as TG
import Core.Type.CoolQ.Update    as CQ
import Utils.Config
import Utils.Logging

import Plugin.Forwarder
import Plugin.BaikeQuerier
import Plugin.NoteSaver
import Plugin.Timer

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
runServer config =
  scotty (port config) $ do
--    middleware logStdoutDev
    handleTGMsg config
    handleCQMsg config

handleTGMsg :: Config -> ScottyM ()
handleTGMsg config =
  Scotty.post (literal "/telegram/") $ do
    update <- jsonData :: ActionM TG.Update
    _ <- liftIO $ fwdTGMsg config update
    _ <- liftIO $ processTGQuery config update
    status status204

handleCQMsg :: Config -> ScottyM ()
handleCQMsg config =
  Scotty.post (literal "/cq/") $ do
    update <- jsonData :: ActionM CQ.Update
    _ <- liftIO $ forkFinally (fwdQQMsg config update) handleExcp
    _ <- liftIO $ forkIO (checkTimer config)
    _ <- liftIO $ forkFinally (processCQQuery config update) handleExcp
    _ <- liftIO $ forkFinally (processNoteOp config update) handleExcp
    _ <- liftIO $ forkFinally (processTimerOp config update) handleExcp
    status status204
  where handleExcp _ = pure ()

