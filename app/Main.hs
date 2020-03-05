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

import Plugin.Forwarder

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
  tgLock <- new :: IO Lock
  cqLock <- new :: IO Lock
  scotty (port config) $ do
    middleware logStdoutDev
    post (literal "/telegram/") $ do
      update <- jsonData :: ActionM TG.Update
      liftIO $ fwdTGtoQQ cqLock (cqServer config) update (groups config)
      status status204
    post (literal "/cq/") $ do
      update <- jsonData :: ActionM CQ.Update
      liftIO $ fwdQQtoTG tgLock (tgbotToken config) update (groups config)
      status status204
