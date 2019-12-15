{-# LANGUAGE OverloadedStrings #-}
module Web.CQ where

import Control.Lens
import Type.Telegram.Update as TG
import Type.CQ.SendMsg      as CQ
import Type.CQ.User         as CQ
import Type.Config
import Network.Wreq
import Data.Aeson
import Data.Text
import Data.Maybe
import Data.Tuple
import Data.Map
import Data.ByteString.Lazy

fwdTgtoQQ :: String -> Update -> GroupMap -> IO (Maybe (Response ByteString))
fwdTgtoQQ cqServer tgUpdate grpMaps = 
  case cqReq of
    Null -> pure Nothing
    _ -> Just <$> postCqRequest cqServer "send_group_msg" cqReq
  where cqReq = transTgGrpUpdate grpMaps tgUpdate

transTgGrpUpdate :: GroupMap -> Update -> Value
transTgGrpUpdate q2tMaps tgUpdate =
  toJSON $ SendMsg <$> targetGrp <*> pure fwdText
    where
      fwdText = Data.Text.concat ["[", user, "] ", content]
      user = first_name ((TG.from.message) tgUpdate)
             <> fromMaybe "" ((last_name.TG.from.message) tgUpdate)
      content = fromMaybe "" ((text.message) tgUpdate)
      fromGrp = (TG.id.chat.message) tgUpdate
      targetGrp = Prelude.lookup fromGrp (swap <$> q2tMaps)

postCqRequest :: String -> String -> Value -> IO (Response ByteString)
postCqRequest cqServer method = post target
  where target = cqServer ++ "/" ++ method
