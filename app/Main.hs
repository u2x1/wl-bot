{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.HTTP.Types                    (status204)
import Network.Wai.Middleware.RequestLogger
import Control.Monad.IO.Class                (liftIO)
import Control.Concurrent.Lock
import Control.Exception                     (try, SomeException)
import Control.Concurrent                    (forkFinally)
import Data.Maybe                            (fromMaybe)
import Data.ByteString.Lazy as B
import Data.Aeson                            (eitherDecode)
import Core.Type.Telegram.Update as TG
import Core.Type.CoolQ.Update    as CQ
import Utils.Config
import Utils.Logging
import Utils.Webhook

import Plugin.Forwarder
import Plugin.BaikeSearcher

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
  setTelegramWebhook config
  tgLock <- new :: IO Lock
  cqLock <- new :: IO Lock
  scotty (port config) $ do
    middleware logStdoutDev
    handleTGMsg config cqLock
    handleCQMsg config cqLock


handleTGMsg :: Config -> Lock -> ScottyM ()
handleTGMsg config lock =
  post (literal "/telegram/") $ do
    update <- jsonData :: ActionM TG.Update
    liftIO $ fwdTGMsg lock config update
    status status204

handleCQMsg :: Config -> Lock -> ScottyM ()
handleCQMsg config lock =
  post (literal "/cq/") $ do
    update <- jsonData :: ActionM CQ.Update
    liftIO $ fwdQQMsg lock config update
    liftIO $ processCQQuery lock config update
    status status204
