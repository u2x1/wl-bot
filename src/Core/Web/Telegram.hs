{-# LANGUAGE OverloadedStrings #-}
module Core.Web.Telegram where

import Network.Wreq
import Data.Aeson
import Data.ByteString.Lazy
import Control.Concurrent
import Control.Concurrent.Lock
import Control.Exception.Base

postTgRequest :: Lock -> String -> String -> Value -> IO ThreadId
postTgRequest = ((.).(.).(.).(.)) forkIO postTgRequestSync

postTgRequestSync :: Lock -> String -> String -> Value -> IO ()
postTgRequestSync lock tgbotTk method jsonContent = do
  acquire lock
  try (post target jsonContent) :: IO (Either SomeException (Response ByteString))
  release lock
  where
    target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method
