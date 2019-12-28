{-# LANGUAGE OverloadedStrings #-}

module Data.Telegram where

import Type.Telegram.Update (message, from, text, first_name, last_name, id, chat, Update)
import Type.CQ.SendMsg
import Type.Config

import Data.Aeson
import Data.Maybe 
import Data.Tuple
import Data.Text            as T

import Prelude hiding (id)

transTgGrpUpdate :: GroupMap -> Update -> Value
transTgGrpUpdate q2tMaps tgUpdate =
  toJSON $ SendMsg <$> targetGrp <*> pure fwdText
    where
      fwdText = T.concat [content, " [", user, "]"]
      targetGrp = lookup fromGrp (swap <$> q2tMaps)
      user = first_name ((from.message) tgUpdate)  <> fromMaybe "" ((last_name.from.message) tgUpdate)
      content = fromMaybe "" ((text.message) tgUpdate)
      fromGrp = (id.chat.message) tgUpdate
