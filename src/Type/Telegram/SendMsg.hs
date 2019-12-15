{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.Telegram.SendMsg 
  (
    SendMsg (SendMsg),
    SendMediaGrpMsg (SendMediaGrpMsg),
    InputMediaPhoto (InputMediaPhoto)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics

data SendMsg = SendMsg {
    chat_id :: Integer
  , text :: Text
  , parse_mode :: Text
--  , reply_to_message_id :: Maybe Integer
} deriving (Eq, Show, Generic)
instance ToJSON SendMsg

data SendMediaGrpMsg = SendMediaGrpMsg {
    chat_id_media :: Integer
  , media :: [InputMediaPhoto]
--  , reply_to_message_id :: Maybe Integer
} deriving (Eq, Show)

data InputMediaPhoto = InputMediaPhoto {
    media_type :: Text
  , media_url :: Text
  , caption :: Text
  , caption_parse_mode :: Text
} deriving (Eq, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = \x -> 
  case x of  
  "chat_id_media" -> "chat_id"
  _ -> x
  } ''SendMediaGrpMsg)

$(deriveJSON defaultOptions {fieldLabelModifier = \x -> 
  case x of  
  "media_type" -> "type"
  "media_url" -> "media"
  "caption_parse_mode" -> "parse_mode"
  _ -> x
  } ''InputMediaPhoto)
