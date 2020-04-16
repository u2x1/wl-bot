{-# LANGUAGE OverloadedStrings #-}

module Core.Data.Telegram where

import Core.Type.Telegram.Update

import Data.Maybe


getMessageFromUpdate :: Update -> (Int, Maybe Message)
getMessageFromUpdate tgUpdate = (msg_type, msg)
  where
    msg = if msg_type > 1 then Nothing else msgs !! msg_type
    msgs = [message, edited_message] <*> pure tgUpdate
    msg_type = Prelude.length $ Prelude.takeWhile isNothing msgs
