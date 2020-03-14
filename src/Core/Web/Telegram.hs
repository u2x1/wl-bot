{-# LANGUAGE OverloadedStrings #-}
module Core.Web.Telegram where

import Network.Wreq
import Data.Aeson
import Data.Text
import Data.ByteString.Lazy

import Core.Type.Unity.Update
import Core.Type.Telegram.Request

import Utils.Config

import Prelude hiding (id)

sendBackTextMsg :: Text -> Update -> Config -> IO (Response ByteString)
sendBackTextMsg textToSend update config =
  postTgRequest tgbotTk "sendMessage" $
            toJSON (SendMsg (chat_id update) textToSend "markdown")
  where tgbotTk = tgbotToken config

postTgRequest :: String -> String -> Value -> IO (Response ByteString)
postTgRequest tgbotTk method = post target
  where
    target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method
