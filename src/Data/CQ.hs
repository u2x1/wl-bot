{-# LANGUAGE OverloadedStrings #-}
module Data.CQ where

import Type.CQ.Update         as CQ
import Type.Telegram.SendMsg
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
  "face"  -> fromJust (CQ.id $ cqdata x)                <> getText cqMsg picCount
  "image" -> T.concat ["[P", pack (show picCount), "]"] <> getText cqMsg (picCount + 1)
  _       -> getText cqMsg picCount
getText [] _ = ""

transfmCqGrpMsgUpdate :: GroupMap -> Update -> Value
transfmCqGrpMsgUpdate q2tMaps cqUpdate =
  case getImgUrls $ message cqUpdate of
    [] -> toJSON $ SendMsg <$> targetGrp <*> pure fwdText <*> Just "HTML"

    imgUrl:[] ->
      let imgcontent = InputMediaPhoto "photo" imgUrl (T.concat ["[<b>", username, "</b>]"]) "HTML" in
      toJSON $ SendMediaGrpMsg <$> targetGrp <*> Just [imgcontent]

    imgUrls   ->
      let imgcontent = fmap (\url -> InputMediaPhoto "photo" url fwdText "HTML") imgUrls in
      toJSON $ SendMediaGrpMsg <$> targetGrp <*> pure imgcontent
    where
      fwdText   = T.concat [(getText (message cqUpdate) 0), " [<b>", username,  "</b>]"]
      username  = nickname (sender cqUpdate)
      targetGrp = group_id cqUpdate >>= flip lookup q2tMaps
