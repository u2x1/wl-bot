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

fwdTGtoQQ :: Lock -> String -> Update -> GroupMap -> IO (Maybe ThreadId)
fwdTGtoQQ lock cqServer tgUpdate grpMaps =
  case cqReq of
    Null -> pure Nothing
    _ -> Just <$> postCqRequestSync lock cqServer "send_group_msg" cqReq
  where cqReq = transTgGrpUpdate grpMaps tgUpdate

postCqRequest :: String -> String -> Value -> IO (Response ByteString)
postCqRequest cqServer method = post target
  where target = cqServer ++ "/" ++ method

postCqRequestSync :: Lock -> String -> String -> Value -> IO ThreadId
postCqRequestSync lock cqServer method jsonContent = do
  wait lock >> acquire lock
  forkFinally (postCqRequest cqServer method jsonContent) handleExp
    where
      handleExp _ = logWT "Info" "Posted a request to CoolQ" >> release lock
