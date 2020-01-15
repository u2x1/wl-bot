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
  let msgs = [message, edited_message] <*> pure tgUpdate
      msg_type = Prelude.length $ Prelude.takeWhile isNothing msgs in
  handleMsgTypes msg_type
    where
      handleMsgTypes mtype =
        case mtype of
          -- Common message
          0 -> toJSON $ SendMsg <$> targetGrp <*> pure fwdText
            where
              common_msg = fromJust $ message tgUpdate
              content = fromMaybe "" (text common_msg)
              targetGrp = lookup fromGrp (swap <$> q2tMaps)
              fromGrp = (id.chat) common_msg
              user = first_name (from common_msg)  <> fromMaybe "" ((last_name.from) common_msg)
              fwdText = T.concat [content, " [", user, "]"]

          -- Edited message
          1 -> toJSON $ SendMsg <$> targetGrp <*> pure fwdText
            where
              edited_msg = fromJust $ edited_message tgUpdate
              fwdText = T.concat ["User[", user, "] edited the following message:\n", content]
              targetGrp = lookup fromGrp (swap <$> q2tMaps)
              fromGrp = (id.chat) edited_msg
              user = first_name (from edited_msg)  <> fromMaybe "" ((last_name.from) edited_msg)
              content = fromMaybe "" (text edited_msg)

