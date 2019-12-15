{-# LANGUAGE OverloadedStrings #-}
module Data.CQ where

import Type.CQ.Update         as CQ
import Type.Telegram.SendMsg
import Type.Config

import Data.Text
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
getText (x:cqMsg) pictureCount = case cqtype x of
                      "text" -> fromJust (text $ cqdata x) <> getText cqMsg pictureCount
                      "face" -> fromJust (CQ.id $ cqdata x) <> getText cqMsg pictureCount
                      "image" -> Data.Text.concat ["[P", pack $ show pictureCount, "]"] <> getText cqMsg (pictureCount + 1)
                      _      -> getText cqMsg pictureCount
getText [] _ = ""

transfmCqGrpMsgUpdate :: GroupMap -> Update -> Value
transfmCqGrpMsgUpdate q2tMaps cqUpdate =
  case getImgUrls $ message cqUpdate of
    []      -> toJSON $ SendMsg <$> targetGrp <*> pure fwdText <*> Just "HTML"
    imgUrls -> toJSON $ SendMediaGrpMsg 
                 <$> targetGrp
                 <*> pure imgcontent
                   where imgcontent = fmap (\url -> InputMediaPhoto "photo" url fwdText "HTML") imgUrls
    where
      fwdText   = Data.Text.concat ["[<b>", username,  "</b>] ", content]
      username  = nickname (sender cqUpdate)
      content   = getText (message cqUpdate) 0
      targetGrp = group_id cqUpdate >>= flip lookup q2tMaps
