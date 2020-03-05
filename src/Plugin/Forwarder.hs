module Plugin.Forwarder where

import Core.Web.Telegram         as TG      (postTgRequest)
import Core.Web.CoolQ            as CQ      (postCqRequest)

import Core.Type.CoolQ.Update    as CQ      (Update, message)
import Core.Type.Telegram.Update as TG      (Update)

import Core.Data.Telegram        as TG      (transTgGrpUpdate)
import Core.Data.CoolQ           as CQ      (getImgRequest, getTextRequest, getImgUrls)

import Control.Concurrent
import Control.Concurrent.Lock
import Utils.Config                         (GroupMap)
import Data.Aeson

fwdQQtoTG :: Lock -> String -> CQ.Update -> GroupMap -> IO (Maybe ThreadId)
fwdQQtoTG lock tgbotTk cqUpdate grpMaps =
  case getTextRequest grpMaps cqUpdate of
    Null -> pure Nothing
    tgReq ->
      case getImgUrls $ CQ.message cqUpdate of
         []      -> Just <$> postTgRequest lock tgbotTk "sendMessage" tgReq
         _:[]    -> Just <$> postTgRequest lock tgbotTk "sendMediaGroup" tgReqWithImg
         imgUrls -> Just <$> do
           postTgRequest lock tgbotTk "sendMessage" tgReq
           postTgRequest lock tgbotTk "sendMediaGroup" tgReqWithImg
         where
           tgReqWithImg = getImgRequest grpMaps cqUpdate

fwdTGtoQQ :: Lock -> String -> TG.Update -> GroupMap -> IO (Maybe ThreadId)
fwdTGtoQQ lock cqServer tgUpdate grpMaps =
  case cqReq of
    Null -> pure Nothing
    _ -> Just <$> postCqRequest lock cqServer "send_group_msg" cqReq
  where cqReq = transTgGrpUpdate grpMaps tgUpdate
