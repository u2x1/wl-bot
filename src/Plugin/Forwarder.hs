module Plugin.Forwarder where

import Core.Web.Telegram         as TG
import Core.Web.CoolQ            as CQ

import Core.Type.CoolQ.Update    as CQ
import Core.Type.Telegram.Update as TG

import Core.Data.Telegram        as TG
import Core.Data.CoolQ           as CQ

import Control.Concurrent
import Utils.Config
import Data.Aeson

fwdQQMsg :: Config -> CQ.Update -> IO (Maybe ThreadId)
fwdQQMsg config cqUpdate
  | not $ forwardOn config = pure Nothing
  | otherwise =
    let grpMaps = groups config
        tgbotTk = tgbotToken config in
    case getTextRequest grpMaps cqUpdate of
      Null -> pure Nothing
      tgReq ->
        case getImgUrls $ CQ.message cqUpdate of
           []      -> Just <$> postTgRequest tgbotTk "sendMessage" tgReq
           _:[]    -> Just <$> postTgRequest tgbotTk "sendMediaGroup" tgReqWithImg
           _ -> Just <$> do
             _ <- postTgRequest tgbotTk "sendMessage" tgReq
             postTgRequest tgbotTk "sendMediaGroup" tgReqWithImg
           where
             tgReqWithImg = getImgRequest grpMaps cqUpdate

fwdTGMsg :: Config -> TG.Update -> IO (Maybe RespBS)
fwdTGMsg config tgUpdate
  | not $ forwardOn config = pure Nothing
  | otherwise =
    let grpMaps = groups config
        cqSvr = cqServer config in
    case transTgGrpUpdate grpMaps tgUpdate of
      Null -> pure Nothing
      req -> Just <$> postCqRequest cqSvr "send_group_msg" req
