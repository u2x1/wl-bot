{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram where

import Type.CoolQ.Update          as CQ     (Update, message)
import Type.Config                          (GroupMap)
import Data.CoolQ                 as CQ     (getImgRequest, getTextRequest, getImgUrls)
import Network.Wreq
import Data.Aeson
import Data.Text
import Data.Maybe
import Data.Tuple
import Data.ByteString.Lazy
import Control.Concurrent
import Control.Concurrent.Lock
import Utils.Logging

fwdQQtoTG :: Lock -> String -> Update -> GroupMap -> IO (Maybe ThreadId)
fwdQQtoTG lock tgbotTk cqUpdate grpMaps =
  case getTextRequest grpMaps cqUpdate of
    Null -> pure Nothing
    tgReq ->
      case getImgUrls $ message cqUpdate of
         []      -> Just <$> postTgRequest lock tgbotTk "sendMessage" tgReq
         _:[]    -> Just <$> postTgRequest lock tgbotTk "sendMediaGroup" tgReqWithImg
         imgUrls -> Just <$> do
           postTgRequest lock tgbotTk "sendMessage" tgReq
           postTgRequest lock tgbotTk "sendMediaGroup" tgReqWithImg
         where
           tgReqWithImg = getImgRequest grpMaps cqUpdate

postTgRequest :: Lock -> String -> String -> Value -> IO ThreadId
postTgRequest = ((.).(.).(.).(.)) (`forkFinally` handleExp) postTgRequestSync
  where
    handleExp _ = logWT "Info" "Posted a request to Telgram"

postTgRequestSync :: Lock -> String -> String -> Value -> IO ()
postTgRequestSync lock tgbotTk method jsonContent = acquire lock >> post target jsonContent >> release lock
  where
    target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method
