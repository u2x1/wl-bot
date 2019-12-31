{-# LANGUAGE OverloadedStrings #-}
module Data.CQ where

import Type.CQ.Update         as CQ
import Type.Telegram.Request
import Type.Config

import Data.Text              as T
import Data.Text.Read
import Data.Either
import Data.Maybe
import Data.Aeson

getImgUrls :: [CQMsg] -> [Text]
getImgUrls (x:cqMsg) = case cqtype x of
  "image" -> fromJust (url $ cqdata x) : getImgUrls cqMsg
  _       -> getImgUrls cqMsg
getImgUrls [] = []

getText :: [CQMsg] -> Int -> Text
getText (x:cqMsg) picCount = case cqtype x of
  "text"  -> fromJust (CQ.text $ cqdata x)              <> getText cqMsg picCount
  "image" -> T.concat ["[P", pack (show picCount), "]"] <> getText cqMsg (picCount + 1)
  _       -> "[[Unsupported Message]]"                  <> getText cqMsg picCount
getText [] _ = ""

-- -- TODO: reply_to_message
-- transfmCqGrpMsgUpdate :: GroupMap -> Update -> Value
-- transfmCqGrpMsgUpdate q2tMaps cqUpdate =
--   case getImgUrls $ message cqUpdate of
--     [] -> toJSON $ SendMsg <$> targetGrp <*> pure fwdText <*> Just "HTML"
--     -- When there is only one photo in a message, hide "[P0]"
--     _:[] ->
--       let imgcontent = InputMediaPhoto "photo" imgUrl (T.concat ["[<b>", username, "</b>]"]) "HTML" in
--       toJSON $ SendMediaGrp <$> targetGrp <*> (pure $ getImgContent cqUpdate)
--     imgUrls   ->
--       let imgcontent = fmap (\url -> InputMediaPhoto "photo" url fwdText "HTML") imgUrls in
--       toJSON $ SendMediaGrp <$> targetGrp <*> pure imgcontent
--     where
--       fwdText   = T.concat [(getText (message cqUpdate) 0), " [<b>", username,  "</b>]"]
--       username  = nickname (sender cqUpdate)
--       targetGrp = group_id cqUpdate >>= flip lookup q2tMaps

getTextRequest :: GroupMap -> Update -> Value
getTextRequest q2tMaps cqUpdate =
  case getImgUrls $ message cqUpdate of
    _ -> toJSON $ SendMsg <$> targetGrp <*> pure fwdText <*> Just "HTML"
   where
     fwdText   = T.concat [(getText (message cqUpdate) 0), " [<b>", username,  "</b>]"]
     username  = nickname (sender cqUpdate)
     targetGrp = group_id cqUpdate >>= flip lookup q2tMaps

getImgRequest :: GroupMap -> Update -> Value
getImgRequest q2tMaps cqUpdate =
  case getImgUrls $ message cqUpdate of
    [] -> Null

    _:[] ->
      toJSON $ SendMediaGrp <$> targetGrp <*> (pure $ getImgContent cqUpdate)

    imgUrls   ->
      toJSON $ SendMediaGrp <$> targetGrp <*> (pure $ getImgContent cqUpdate)
        where
          fwdText   = T.concat [(getText (message cqUpdate) 0), " [<b>", username,  "</b>]"]
    where
      username  = nickname (sender cqUpdate)
      targetGrp = group_id cqUpdate >>= flip lookup q2tMaps

getImgContent :: Update -> [InputMediaPhoto]
getImgContent cqUpdate = fmap (\url -> InputMediaPhoto "photo" url "" "HTML") imgUrls
  where imgUrls = getImgUrls $ message cqUpdate
