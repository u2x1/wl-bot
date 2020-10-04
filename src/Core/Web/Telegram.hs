{-# LANGUAGE OverloadedStrings #-}
module Core.Web.Telegram where

import           Data.Aeson
import           Data.ByteString.Lazy
import           Network.Wreq

import           Prelude              hiding (id)

postTgRequest :: String -> String -> Value -> IO (Response ByteString)
postTgRequest tgbotTk method = post target
  where
    target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method

postTgRequest' :: String -> String -> [Part] -> IO (Response ByteString)
postTgRequest' tgbotTk method = post target
  where
    target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method
