{-# LANGUAGE OverloadedStrings #-}
module Core.Web.Telegram where

import           Data.Aeson                     ( Value )
import           Data.ByteString.Lazy           ( ByteString )
import           Network.Wreq                   ( Part
                                                , Response
                                                , post
                                                )

import           Prelude                 hiding ( id )

postTgRequest :: String -> String -> Value -> IO (Response ByteString)
postTgRequest tgbotTk method = post target
  where target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method

postTgRequest' :: String -> String -> [Part] -> IO (Response ByteString)
postTgRequest' tgbotTk method = post target
  where target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method
