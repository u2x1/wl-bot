{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Type.Telegram.Update where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

import           Utils.Json

data Update = Update {
    update_id      :: Integer
  , message        :: Maybe Message
  , edited_message :: Maybe Message
} deriving (Eq, Show, Generic)
instance FromJSON Update

data Message = Message {
    tgm_message_id :: Integer
  , tgm_from       :: User
  , tgm_chat       :: Chat
  , tgm_text       :: Maybe String
  , tgm_reply_id   :: Maybe Integer
} deriving (Eq, Show, Generic)
instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> Message
    <$> (v .: "message_id")
    <*> (v .: "from")
    <*> (v .: "chat")
    <*> (v .: "text")
    <*> ((v .: "reply_to_message") >>= (.:? "message_id"))

data Chat = Chat {
    chat_id   :: Integer
  , chat_type :: String
} deriving (Eq, Show, Generic)
instance FromJSON Chat where
  parseJSON = dropParseJSON 5

data User = User {
    user_first_name :: Text
  , user_last_name  :: Maybe Text
  , user_id         :: Integer
} deriving (Eq, Show, Generic)
instance FromJSON User where
  parseJSON = dropParseJSON 5
