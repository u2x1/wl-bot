{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram where

import Type.CQ.Update             as CQ     (Update, message)
import Type.Config                          (GroupMap)
import Data.CQ                    as CQ     (getImgRequest, getTextRequest, getImgUrls)
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
        []      -> Just <$> postTgRequestSync lock tgbotTk "sendMessage" tgReq
        imgUrls -> Just <$> do
          postTgRequestSync lock tgbotTk "sendMessage" tgReq
          postTgRequestSync lock tgbotTk "sendMediaGroup" tgReqWithImg
            where tgReqWithImg = getImgRequest grpMaps cqUpdate

postTgRequest :: String -> String -> Value -> IO (Response ByteString)
postTgRequest tgbotTk method = post target
  where target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method

postTgRequestSync :: Lock -> String -> String -> Value -> IO ThreadId
postTgRequestSync lock tgbotTk method jsonContent= do
  wait lock >> acquire lock
  forkFinally (postTgRequest tgbotTk method jsonContent) handleExp
  where
    handleExp _ = logWT "Info" "Posted a request to Telgram" >> release lock
