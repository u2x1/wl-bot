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

fwdTgtoQQ :: String -> Update -> GroupMap -> IO (Maybe (Response ByteString))
fwdTgtoQQ cqServer tgUpdate grpMaps = 
  case cqReq of
    Null -> pure Nothing
    _ -> Just <$> postCqRequest cqServer "send_group_msg" cqReq
  where cqReq = transTgGrpUpdate grpMaps tgUpdate

postCqRequest :: String -> String -> Value -> IO (Response ByteString)
postCqRequest cqServer method = post target
  where target = cqServer ++ "/" ++ method
