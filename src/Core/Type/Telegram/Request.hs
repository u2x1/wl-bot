{-# LANGUAGE DeriveGeneric #-}

module Core.Type.Telegram.Request
  (
    SendMsg (SendMsg),
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Utils.Json

data SendMsg = SendMsg {
    send_msg_chat_id :: Integer
  , send_msg_text :: Maybe Text
  , send_msg_photo :: Maybe Text
  , send_msg_caption :: Maybe Text
  , send_msg_parse_mode :: String
  , send_msg_reply_to_message_id :: Maybe Integer
} deriving (Eq, Show, Generic)

instance ToJSON SendMsg where
  toJSON = dropToJSON 9

-- data SendMediaGrp = SendMediaGrp {
--     media_grp_chat_id :: Integer
--   , meida_grp_media :: [InputMediaPhoto]
--   , media_grp_reply_to_message_id :: Maybe Integer
-- } deriving (Eq, Show, Generic)
-- 
-- instance ToJSON SendMediaGrp where
--   toJSON = dropToJSON 10
-- 
-- data InputMediaPhoto = InputMediaPhoto {
--     input_media_photo_type :: Text
--   , input_media_photo_media :: Text
--   , input_media_photo_caption :: Text
--   , input_media_photo_parse_mode :: Text
-- } deriving (Eq, Show, Generic)
-- 
-- instance ToJSON InputMediaPhoto where
--   toJSON = dropToJSON 18
