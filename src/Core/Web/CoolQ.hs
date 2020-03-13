{-# LANGUAGE OverloadedStrings #-}
module Core.Web.CoolQ where

import Network.Wreq
import Data.Aeson
import Data.Text
import Data.ByteString.Lazy
import Data.Maybe
import Control.Concurrent

import Core.Type.CoolQ.Update
import Core.Type.CoolQ.SendMsg

import Utils.Logging
import Utils.Config

type RespBS = Response ByteString

sendBackTextMsg :: Text -> Update -> Config -> IO RespBS
sendBackTextMsg textToSend cqUpdate config =
  let cqSvr = cqServer config in
  if isNothing $ group_id cqUpdate
     then sendPrivMsg (fromJust $ user_id cqUpdate) textToSend config
     else sendGrpMsg (fromJust $ group_id cqUpdate) textToSend config

sendPrivMsg :: Integer -> Text -> Config -> IO RespBS
sendPrivMsg userId textToSend config =
  postCqRequest (cqServer config) "send_private_msg" (toJSON (SendPrivMsg userId textToSend))

sendGrpMsg :: Integer -> Text -> Config -> IO RespBS
sendGrpMsg groupId textToSend config =
  postCqRequest (cqServer config) "send_group_msg" (toJSON (SendGrpMsg groupId textToSend))

postCqRequest :: String -> String -> Value -> IO RespBS
postCqRequest cqSvr method = post (cqSvr ++ "/" ++ method)
