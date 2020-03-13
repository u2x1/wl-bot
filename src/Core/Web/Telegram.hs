{-# LANGUAGE OverloadedStrings #-}
module Core.Web.Telegram where

import Network.Wreq
import Data.Aeson
import Data.Text
import Data.Maybe
import Control.Concurrent

import Core.Type.Telegram.Update
import Core.Type.Telegram.Request

import Utils.Config
import Utils.Logging

import Prelude hiding (id)

sendBackTextMsg :: Text -> Update -> Config -> IO ThreadId
sendBackTextMsg textToSend tgUpdate config =
  let tgbotTk = tgbotToken config
      msgs = [message, edited_message] <*> pure tgUpdate
      msg_type = Prelude.length $ Prelude.takeWhile isNothing msgs in
  if msg_type < 3
     then postTgRequest tgbotTk "sendMessage" $
            toJSON (SendMsg ((id.chat) $ fromJust $ msgs !! msg_type) textToSend "markdown")
     else forkIO (logWT Warning "Message type not supported")

postTgRequest :: String -> String -> Value -> IO ThreadId
postTgRequest tgbotTk method jsonContent = forkFinally (post target jsonContent) handleExcp
  where
    handleExcp (Left err) = logErr (show err) "Failed to post requests to Telegram"
    handleExcp _ = pure ()
    target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method
