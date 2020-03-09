{-# LANGUAGE OverloadedStrings #-}
module Core.Web.Telegram where

import Network.Wreq
import Data.Aeson
import Data.ByteString.Lazy
import Control.Concurrent
import Control.Exception.Base

import Utils.Logging

postTgRequest :: String -> String -> Value -> IO ThreadId
postTgRequest tgbotTk method jsonContent = forkFinally (post target jsonContent) handleExcp
  where
    handle (Left _) = logWT "ERROR" "Failed to post requests to Telegram."
    handleExcp (Right _) =  pure ()
    target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method
