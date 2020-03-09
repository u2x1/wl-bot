{-# LANGUAGE OverloadedStrings #-}
module Core.Web.CoolQ where

import Network.Wreq
import Data.Aeson
import Data.ByteString.Lazy
import Data.Text
import Data.Maybe
import Control.Concurrent
import Control.Exception.Base

import Core.Type.CoolQ.Update
import Core.Type.CoolQ.SendMsg

import Utils.Logging
import Utils.Config

sendBackMsg :: Text -> Update -> Config -> IO ThreadId
sendBackMsg textToSend cqUpdate config =
  let cqSvr = cqServer config in
  if isNothing $ group_id cqUpdate
     then  postCqRequest cqSvr "send_private_msg"
                    (toJSON (SendPrivMsg <$> user_id cqUpdate <*> Just textToSend))
     else  postCqRequest cqSvr "send_group_msg"
                    (toJSON (SendGrpMsg <$> group_id cqUpdate <*> Just textToSend))

postCqRequest :: String -> String -> Value -> IO ThreadId
postCqRequest cqServer method jsonContent = forkFinally (post target jsonContent) handleExcp
    where
      handleExcp (Left _) = logWT "ERROR" "Failed to post requests to CoolQ."
      handleExcp (Right _) =  pure ()
      target = cqServer ++ "/" ++ method
