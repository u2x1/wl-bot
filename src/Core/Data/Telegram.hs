{-# LANGUAGE OverloadedStrings #-}

module Core.Data.Telegram where

import Core.Type.Telegram.Update as T
import Core.Type.Telegram.Request as T
import Core.Type.Unity.Request as UR
import Network.Wreq (Part, partFile, partText)
import Data.Text (pack)

import Data.Maybe

getMessageFromUpdate :: T.Update -> (Int, Maybe Message)
getMessageFromUpdate tgUpdate = (msg_type, msg)
  where
    msg = if msg_type > 1 then Nothing else msgs !! msg_type
    msgs = [message, edited_message] <*> pure tgUpdate
    msg_type = Prelude.length $ Prelude.takeWhile isNothing msgs

transMsg :: UR.SendMsg -> Either [Part] T.SendMsg
transMsg msg
  | isJust $ imgPath msg = Left [partText "chat_id" (pack.show $ UR.chat_id msg), partText "reply_to_message_id" (pack.show $ UR.reply_id msg), partFile "photo" (fromJust $ ("images/"<>) <$>imgPath msg)]
  | isJust $ imgUrl msg = Right (T.SendMsg (UR.chat_id msg) Nothing (imgUrl msg) (UR.text msg) "HTML" (reply_id msg))
  | otherwise = Right (T.SendMsg (UR.chat_id msg) (UR.text msg) Nothing Nothing "HTML" (reply_id msg))
