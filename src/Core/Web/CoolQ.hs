{-# LANGUAGE OverloadedStrings #-}
module Core.Web.CoolQ where

import Network.Wreq
import Data.Aeson
import Data.Text
import Data.Maybe
import Control.Concurrent

import Core.Type.CoolQ.Update
import Core.Type.CoolQ.SendMsg

import Utils.Logging
import Utils.Config

sendBackTextMsg :: Text -> Update -> Config -> IO ThreadId
sendBackTextMsg textToSend cqUpdate config =
  let cqSvr = cqServer config in
  if isNothing $ group_id cqUpdate
     then  postCqRequest cqSvr "send_private_msg"
                    (toJSON (SendPrivMsg <$> user_id cqUpdate <*> Just textToSend))
     else  postCqRequest cqSvr "send_group_msg"
                    (toJSON (SendGrpMsg <$> group_id cqUpdate <*> Just textToSend))

postCqRequest :: String -> String -> Value -> IO ThreadId
postCqRequest cqSvr method jsonContent = forkFinally (post target jsonContent) handleExcp
    where
      handleExcp (Left err) = logErr (show err) "Failed to post requests to CoolQ."
      handleExcp _ = pure ()
      target = cqSvr ++ "/" ++ method
