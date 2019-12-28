{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram where

import Type.CQ.Update             as CQ     (Update, message)
import Type.Config                          (GroupMap)
import Data.CQ                    as CQ     (transfmCqGrpMsgUpdate, getImgUrls)
import Network.Wreq
import Data.Aeson
import Data.Text
import Data.Maybe
import Data.Tuple
import Data.ByteString.Lazy
import Control.Concurrent
import Control.Concurrent.Lock
import Utils.Logging

fwdQQtoTGsync :: Lock -> String -> CQ.Update -> GroupMap -> IO ThreadId
fwdQQtoTGsync lock tgbotTk cqUpdate grpMaps = do
  wait lock
  acquire lock
  forkFinally (fwdQQtoTG tgbotTk cqUpdate grpMaps) handleExp
  where
   handleExp (Right _) = logWT "Info" "Forwarded a message to TG" >> release lock
   handleExp (Left err)  = logWT "Error" ("Failed to forward a message: " <> show err) >> release lock

fwdQQtoTG :: String -> Update -> GroupMap -> IO (Maybe (Response ByteString))
fwdQQtoTG tgbotTk cqUpdate grpMaps =
  case transfmCqGrpMsgUpdate grpMaps cqUpdate of
    Null -> pure Nothing
    tgReq ->
      case getImgUrls $ message cqUpdate of
        []      -> Just <$> postTgRequest tgbotTk "sendMessage" tgReq
        imgUrls -> Just <$> postTgRequest tgbotTk "sendMediaGroup" tgReq

postTgRequest :: String -> String -> Value -> IO (Response ByteString)
postTgRequest tgbotTk method = post target
  where target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method
