{-# LANGUAGE OverloadedStrings #-}

module Data.Telegram where

import Type.Telegram.Update    as TG
import Type.CQ.SendMsg
import Type.Config

import Data.Aeson
import Data.Maybe 
import Data.Tuple
import Data.Text         as T

transTgGrpUpdate :: GroupMap -> Update -> Value
transTgGrpUpdate q2tMaps tgUpdate =
  toJSON $ SendMsg <$> targetGrp <*> pure fwdText
    where
      fwdText = T.concat ["[", user, "] ", content]
      user = first_name ((TG.from.message) tgUpdate)
             <> fromMaybe "" ((last_name.TG.from.message) tgUpdate)
      content = fromMaybe "" ((text.message) tgUpdate)
      fromGrp = (TG.id.chat.message) tgUpdate
      targetGrp = Prelude.lookup fromGrp (swap <$> q2tMaps)
