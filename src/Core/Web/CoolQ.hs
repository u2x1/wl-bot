{-# LANGUAGE OverloadedStrings #-}
module Core.Web.CoolQ where

import Network.Wreq
import Data.Aeson
import Data.Text
import Data.ByteString.Lazy

import Core.Type.Unity.Update
import Core.Type.CoolQ.SendMsg
import Core.Type.Universal

import Utils.Config

sendBackTextMsg :: Text -> Update -> Config -> IO (Response ByteString)
sendBackTextMsg textToSend update config =
  case message_type update of
     Private -> sendPrivMsg (user_id update) textToSend config
     Group   -> sendGrpMsg (chat_id update) textToSend config

sendPrivMsg :: Integer -> Text -> Config -> IO (Response ByteString)
sendPrivMsg userId textToSend config =
  postCqRequest (cqServer config) "send_private_msg" (toJSON (SendPrivMsg userId textToSend))

sendGrpMsg :: Integer -> Text -> Config -> IO (Response ByteString)
sendGrpMsg groupId textToSend config =
  postCqRequest (cqServer config) "send_group_msg" (toJSON (SendGrpMsg groupId textToSend))

postCqRequest :: String -> String -> Value -> IO (Response ByteString)
postCqRequest cqSvr method = post (cqSvr ++ "/" ++ method)
