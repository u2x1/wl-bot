{-# LANGUAGE OverloadedStrings #-}
module Web.CQ where

import Control.Lens
import Type.Telegram.Update as TG
import Type.CQ.User         as CQ
import Type.Config
import Data.Telegram
import Network.Wreq
import Data.Aeson
import Data.ByteString.Lazy
import Control.Concurrent
import Control.Concurrent.Lock
import Utils.Logging

fwdTGtoQQsync :: Lock -> String -> TG.Update -> GroupMap -> IO ThreadId
fwdTGtoQQsync lock cqServer tgUpdate grpMaps = do
  wait lock
  acquire lock
  forkFinally (fwdTGtoQQ cqServer tgUpdate grpMaps) handleExp
  where
    handleExp (Right _) = logWT "Info" "Forwarded a message to QQ" >> release lock
    handleExp (Left err)  = logWT "Error" ("Failed to forward a message: " <> show err) >> release lock

fwdTGtoQQ :: String -> Update -> GroupMap -> IO (Maybe (Response ByteString))
fwdTGtoQQ cqServer tgUpdate grpMaps =
  case cqReq of
    Null -> pure Nothing
    _ -> Just <$> postCqRequest cqServer "send_group_msg" cqReq
  where cqReq = transTgGrpUpdate grpMaps tgUpdate

postCqRequest :: String -> String -> Value -> IO (Response ByteString)
postCqRequest cqServer method = post target
  where target = cqServer ++ "/" ++ method
