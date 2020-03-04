{-# LANGUAGE OverloadedStrings #-}

module Data.Telegram where

import Type.Telegram.Update
import Type.CoolQ.SendMsg
import Type.Config

import Data.Aeson
import Data.Maybe
import Data.Tuple
import Data.Foldable (asum)
import Data.Text            as T

import Prelude hiding (id)

transTgGrpUpdate :: GroupMap -> Update -> Value
transTgGrpUpdate q2tMaps tgUpdate =
  toJSON $ SendMsg <$> fst msgInfo <*> pure (snd msgInfo)
  where
    msgInfo = handleMsg msg_type msgs q2tMaps
    msgs = [message, edited_message] <*> pure tgUpdate
    msg_type = Prelude.length $ Prelude.takeWhile isNothing msgs

handleMsg mtype msgs q2tMaps = (,) targetGrp (
  case mtype of
    -- Common message
    0 -> T.concat [user, "> ", content]
    -- Edited message
    1 -> T.concat ["[", user, "] edited the following message:\n", content] )
  where
    content = fromMaybe "" (text msg)
    msg = fromJust $ asum msgs
    targetGrp = lookup fromGrp (swap <$> q2tMaps)
    fromGrp = (id.chat) msg
    user = first_name (from msg)  <> fromMaybe "" ((last_name.from) msg)
