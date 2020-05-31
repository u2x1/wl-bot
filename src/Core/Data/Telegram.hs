{-# LANGUAGE OverloadedStrings #-}

module Core.Data.Telegram where

import Core.Type.Telegram.Update  as TU (Update, Message, message, edited_message)
import Core.Type.Telegram.Request as TR (SendMsg(SendMsg))
import Core.Type.Unity.Request    as UR
import Data.Text                        (pack)
import Data.Text.Lazy                   (toStrict)
import Data.Text.Lazy.Builder           (toLazyText)
import Data.Maybe                       (fromJust, isNothing, isJust)
import HTMLEntities.Decoder             (htmlEncodedText)
import Network.Wreq                     (Part, partFile, partText)

-- | Get Telegram Message (Message or EditedMessage) from Telegram Update
getMessageFromUpdate :: TU.Update -> (Int, Maybe Message)
getMessageFromUpdate tgUpdate = (msg_type, msg)
  where
    msg = if msg_type > 1 then Nothing else msgs !! msg_type
    msgs = [message, edited_message] <*> pure tgUpdate
    msg_type = Prelude.length $ Prelude.takeWhile isNothing msgs

-- | Transform SendMsg into [Part] when local image upload is required.
transMsg :: UR.SendMsg -> Either [Part] TR.SendMsg
transMsg msg
  | isJust $ imgPath msg =
      Left [ partText "chat_id" (pack.show $ UR.chat_id msg)
           , partText "reply_to_message_id" (pack.show $ UR.reply_id msg)
           , partFile "photo" (fromJust $ ("images/"<>) <$>imgPath msg)]
  | isJust $ imgUrl msg =
      Right (TR.SendMsg (UR.chat_id msg) Nothing (imgUrl msg) (UR.text msg) "HTML" (reply_id msg))
  | otherwise =
      Right (TR.SendMsg (UR.chat_id msg) (decodeHtml <$> UR.text msg) Nothing Nothing "HTML" (reply_id msg))
  where
    decodeHtml = toStrict.toLazyText.htmlEncodedText
