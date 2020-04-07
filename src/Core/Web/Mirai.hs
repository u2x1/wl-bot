{-# LANGUAGE OverloadedStrings #-}
module Core.Web.Mirai where

import Control.Lens
import Network.Wreq
import Data.Aeson
import Data.ByteString.Lazy
import Core.Type.Mirai.Request
import Utils.Config

sendGrpMsg :: Integer -> [Message] -> Config -> Maybe Integer -> IO (Response ByteString)
sendGrpMsg groupId msgs config replyId =
  postCqRequest (config ^. mirai_server) "sendGroupMessage"
    (toJSON (SendMRMsg groupId (config ^. mirai_session_key) replyId msgs))

sendPrivMsg :: Integer -> [Message] -> Config -> Maybe Integer -> IO (Response ByteString)
sendPrivMsg userId msgs config replyId =
  postCqRequest (config ^. mirai_server) "sendFriendMessage"
    (toJSON (SendMRMsg userId (config ^. mirai_session_key) replyId msgs))

postCqRequest :: String -> String -> Value -> IO (Response ByteString)
postCqRequest cqSvr method = postWith opts (cqSvr ++ method)
  where opts = defaults & header "Content-Type" .~ ["text/plain; charset=UTF-8"]
