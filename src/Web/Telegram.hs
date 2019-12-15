{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram where

import Type.Telegram.SendMsg
import Type.CQ.Update
import Type.Config
import Data.CQ
import Network.Wreq
import Data.Aeson
import Data.Text
import Data.Maybe
import Data.Tuple
import Data.ByteString.Lazy
import Utils.Logging

fwdQQtoTg :: String -> Update -> GroupMap -> IO (Maybe (Response ByteString))
fwdQQtoTg tgbotTk cqUpdate grpMaps = 
  case tgReq of
    Null -> pure Nothing
    _ ->
      case getImgUrls $ message cqUpdate of
        []      -> Just <$> postTgRequest tgbotTk "sendMessage" tgReq
        imgUrls -> Just <$> postTgRequest tgbotTk "sendMediaGroup" tgReq
  where tgReq = transfmCqGrpMsgUpdate grpMaps cqUpdate

postTgRequest :: String -> String -> Value -> IO (Response ByteString)
postTgRequest tgbotTk method = post target
  where target = "https://api.telegram.org/bot" ++ tgbotTk ++ "/" ++ method
