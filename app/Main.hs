{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty                as Scotty
import Network.HTTP.Types                    (status204)
-- import Network.Wai.Middleware.RequestLogger
import Control.Monad.IO.Class                (liftIO)
import Control.Exception                     (try, SomeException)
--import Control.Concurrent                    (forkFinally)
import Data.ByteString.Lazy      as BL
import Data.Aeson                            (eitherDecode)
import Core.Type.Telegram.Update as TG
import Core.Type.CoolQ.Update    as CQ
import Utils.Config
import Utils.Logging
--import Utils.Webhook

import Plugin.Forwarder
import Plugin.BaikeQuerier
import Plugin.NoteSaver

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
--  _ <- forkFinally (setTelegramWebhook config) handleExcp
  scotty (port config) $ do
--    middleware logStdoutDev
    handleTGMsg config
    handleCQMsg config
  where
    handleExcp (Left excp) = logWT Error ("Failed setting webhook: " <> show excp)
    handleExcp (Right _) = pure ()

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
    _ <- liftIO $ fwdQQMsg config update
    _ <- liftIO $ processCQQuery config update
    _ <- liftIO $ processNoteOp config update
    status status204
