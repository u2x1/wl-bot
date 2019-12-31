{-# LANGUAGE OverloadedStrings #-}

module Data.Telegram where

import Type.Telegram.Update 
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
      user = first_name (from msg)  <> fromMaybe "" ((last_name.from) msg)
      content = fromMaybe "" (text msg)
      fromGrp = (id.chat) msg
      msg = case message tgUpdate of
              Just new_msg -> new_msg
              Nothing -> case edited_message tgUpdate of
                           Just edited_msg -> edited_msg
