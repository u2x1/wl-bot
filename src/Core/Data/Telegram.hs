{-# LANGUAGE OverloadedStrings #-}

module Core.Data.Telegram where

import           Control.Lens                   ( (^.) )
import           Core.Type.Telegram.Request    as TR
                                                ( SendMsg(SendMsg) )
import           Core.Type.Telegram.Update     as TU
                                                ( Message
                                                , Update
                                                , edited_message
                                                , message
                                                )
import           Core.Type.Unity.Request       as UR
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                )
import           Data.Text                      ( pack )
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text.Lazy.Builder         ( toLazyText )
import           HTMLEntities.Decoder           ( htmlEncodedText )
import           Network.Wreq                   ( Part
                                                , partFile
                                                , partText
                                                )

-- | Get Telegram Message (Message or EditedMessage) from Telegram Update
getMessageFromUpdate :: TU.Update -> (Int, Maybe Message)
getMessageFromUpdate tgUpdate = (msg_type, msg)
 where
  msg      = if msg_type > 1 then Nothing else msgs !! msg_type
  msgs     = [message, edited_message] <*> pure tgUpdate
  msg_type = Prelude.length $ Prelude.takeWhile isNothing msgs

-- | Transform SendMsg into [Part] when local image upload is required.
transMsg :: UR.SendMsg -> Either [Part] TR.SendMsg
transMsg msg
  | isJust $ msg ^. imgPath = Left
    [ partText "chat_id"             (pack . show $ msg ^. UR.chat_id)
    , partText "reply_to_message_id" (pack . show $ msg ^. UR.reply_id)
    , partFile "photo" (fromJust $ ("images/" <>) <$> msg ^. imgPath)
    ]
  | isJust $ msg ^. imgUrl = Right $ TR.SendMsg (msg ^. UR.chat_id)
                                                Nothing
                                                (msg ^. imgUrl)
                                                (msg ^. UR.text)
                                                "HTML"
                                                (msg ^. reply_id)
                                                True
  | otherwise = Right $ TR.SendMsg (msg ^. UR.chat_id)
                                   (decodeHtml <$> msg ^. UR.text)
                                   Nothing
                                   Nothing
                                   "HTML"
                                   (msg ^. reply_id)
                                   True
  where decodeHtml = toStrict . toLazyText . htmlEncodedText
