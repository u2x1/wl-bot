{-# LANGUAGE OverloadedStrings #-}
module Core.Web.Mirai where

import Core.Data.Mirai ( wrapWS )
import Core.Type.Mirai.Request ( Message, SendMRMsg(SendMRMsg) )
import Data.Aeson ( ToJSON(toJSON), Value, encode )
import qualified Network.WebSockets            as WS
import Utils.Env ( Env(wsConn) )

sendGrpMsg :: String -> [Message] -> Env -> Maybe String -> IO ()
sendGrpMsg grpId msgs env replyId =
  postMiraiRequest (wsConn env) $ toJSON $ wrapWS
    "sendGroupMessage"
    (SendMRMsg Nothing (Just grpId) replyId msgs)


sendTempMsg :: String -> String -> [Message] -> Env -> IO ()
sendTempMsg userId grpId msgs env =
  postMiraiRequest (wsConn env) $ toJSON $ wrapWS
    "sendTempMessage"
    (SendMRMsg (Just userId) (Just grpId) Nothing msgs)

sendPrivMsg :: String -> [Message] -> Env -> IO ()
sendPrivMsg userId msgs env = postMiraiRequest (wsConn env) $ toJSON $ wrapWS
  "sendFriendMessage"
  (SendMRMsg (Just userId) Nothing Nothing msgs)

postMiraiRequest :: WS.Connection -> Value -> IO ()
postMiraiRequest conn value = WS.sendTextData conn (encode value)
