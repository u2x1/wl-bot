{-# LANGUAGE OverloadedStrings #-}

module Data.Telegram where

import Type.Telegram.Update
import Type.CoolQ.SendMsg
import Type.Config

import Data.Aeson
import Data.Maybe
import Data.Tuple
import Data.Text            as T

import Prelude hiding (id)

transTgGrpUpdate :: GroupMap -> Update -> Value
transTgGrpUpdate q2tMaps tgUpdate =
  case message tgUpdate of
    Just new_msg -> toJSON $ SendMsg <$> targetGrp <*> pure fwdText
      where
        content = fromMaybe "" (text new_msg)
        targetGrp = lookup fromGrp (swap <$> q2tMaps)
        fromGrp = (id.chat) new_msg
        user = first_name (from new_msg)  <> fromMaybe "" ((last_name.from) new_msg)
        fwdText = T.concat [content, " [", user, "]"]
    Nothing -> case edited_message tgUpdate of
                 Just edited_msg -> toJSON $ SendMsg <$> targetGrp <*> pure fwdText
                   where
                     fwdText = T.concat ["User[", user, "] edited the following message:\n", content]
                     targetGrp = lookup fromGrp (swap <$> q2tMaps)
                     fromGrp = (id.chat) edited_msg
                     user = first_name (from edited_msg)  <> fromMaybe "" ((last_name.from) edited_msg)
                     content = fromMaybe "" (text edited_msg)
