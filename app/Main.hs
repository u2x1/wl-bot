{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty                as Scotty
import Network.HTTP.Types                    (status204)
import Network.Wai.Middleware.RequestLogger
import Control.Monad.IO.Class                (liftIO)
import Control.Exception                     (try, SomeException)
import Control.Concurrent                    (forkFinally)
import Data.ByteString.Lazy as B
import Data.Aeson                            (eitherDecode)
import Core.Type.Telegram.Update as TG
import Core.Type.CoolQ.Update    as CQ
import Utils.Config
import Utils.Logging
import Utils.Webhook

import Plugin.Forwarder
import Plugin.BaikeQuerier

main :: IO ()
main = do
  cb <- try $ B.readFile "config.json" :: IO (Either SomeException ByteString)
  case cb of
    Right c ->
      case eitherDecode c :: Either String Config of
        Right config -> runServer config
        Left err -> logWT "Error" ("Failed parsing config file: " <> err)
    Left err -> logWT "Error" ("Failed opening config file: " <> show err)

runServer :: Config -> IO ()
runServer config = do
  _ <- forkFinally (setTelegramWebhook config) handleExcp
  scotty (port config) $ do
    middleware logStdoutDev
    handleTGMsg config
    handleCQMsg config
  where
    handleExcp  (Left excp) = logWT "Error" ("Failed setting webhook: " <> show excp)
    handleExcp  (Right _) = pure ()

handleTGMsg :: Config -> ScottyM ()
handleTGMsg config =
  Scotty.post (literal "/telegram/") $ do
    update <- jsonData :: ActionM TG.Update
    _ <- liftIO $ fwdTGMsg config update
    status status204

handleCQMsg :: Config -> ScottyM ()
handleCQMsg config =
  Scotty.post (literal "/cq/") $ do
    update <- jsonData :: ActionM CQ.Update
    _ <- liftIO $ fwdQQMsg config update
    _ <- liftIO $ processCQQuery config update
    status status204
