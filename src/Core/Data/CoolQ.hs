{-# LANGUAGE OverloadedStrings #-}
module Core.Data.CoolQ where

import Core.Type.CoolQ.Update         as CQ
import Core.Type.Telegram.Request
import Utils.Config

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

getText :: [CQMsg] -> Text
getText msg = go msg 0
  where
    go (x:cqMsg) picCount = case cqtype x of
      "text"  -> fromJust (CQ.text $ cqdata x)              <> go cqMsg picCount
      "image" -> T.concat ["[P", pack (show picCount), "]"] <> go cqMsg (picCount + 1)
      _       -> "[[Unsupported Message]]"                  <> go cqMsg picCount
    go [] _ = ""

getTextRequest :: GroupMap -> Update -> Value
getTextRequest q2tMaps cqUpdate =
   toJSON $ SendMsg <$> targetGrp <*> pure fwdText <*> Just "HTML"
   where
     fwdText   = T.concat ["<b>", username, "</b>&gt; ", getText (message cqUpdate)]
     username  = nickname (sender cqUpdate)
     targetGrp = group_id cqUpdate >>= flip lookup q2tMaps

getImgRequest :: GroupMap -> Update -> Value
getImgRequest q2tMaps cqUpdate = toJSON $ SendMediaGrp <$> targetGrp <*> (pure $ getImgContent cqUpdate)
  where
    targetGrp = group_id cqUpdate >>= flip lookup q2tMaps

getImgContent :: Update -> [InputMediaPhoto]
getImgContent cqUpdate =
  fmap (\url -> InputMediaPhoto "photo" url fwdText "HTML") imgUrls
  where
    imgUrls = getImgUrls $ message cqUpdate
    fwdText = case imgUrls of
                -- If there is only one photo in a message, hide "[P0]"
                _:[] -> T.concat ["[<b>", username,  "</b>]"]
                _    -> T.concat ["<b>", username, "</b>&gt; ", getText (message cqUpdate)]
                where
                  username  = nickname (sender cqUpdate)
