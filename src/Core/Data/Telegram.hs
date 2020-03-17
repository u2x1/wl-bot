{-# LANGUAGE OverloadedStrings #-}

module Core.Data.Telegram where

import Core.Type.Telegram.Update

import Data.Aeson
import Data.Maybe
import Data.Tuple
import Data.Foldable (asum)
import Data.Text            as T

import Prelude hiding (id)

getMessageFromUpdate :: Update -> (Int, Maybe Message)
getMessageFromUpdate tgUpdate = (msg_type, msg)
  where
    msg = if msg_type > 1 then Nothing else msgs !! msg_type
    msgs = [message, edited_message] <*> pure tgUpdate
    msg_type = Prelude.length $ Prelude.takeWhile isNothing msgs

handleMsg :: (Eq a, Num a, Foldable t) =>
     a
     -> t (Maybe Message)
     -> [(b, Integer)]
     -> (Maybe b, Text)
handleMsg mtype msgs q2tMaps = (,) targetGrp $
  case mtype of
    -- Common message
    0 -> T.concat [user, "> ", content]
    -- Edited message
    1 -> T.concat ["[", user, "] edited the following message:\n", content]
    -- Make GHC not complain
    _ -> "WTF"
  where
    content = fromMaybe "" (text msg)
    msg = fromJust $ asum msgs
    targetGrp = lookup fromGrp (swap <$> q2tMaps)
    fromGrp = (chat_id.chat) msg
    user = user_first_name (from msg)  <> fromMaybe "" ((user_last_name.from) msg)
