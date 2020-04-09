{-# LANGUAGE DeriveGeneric #-}
module Core.Type.Telegram.Update where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

import Utils.Json

data Update = Update {
    update_id :: Integer
  , message :: Maybe Message
  , edited_message :: Maybe Message
} deriving (Eq, Show, Generic)
instance FromJSON Update

data Message = Message {
    message_id :: Integer
  , from :: User
  , chat :: Chat
  , text :: Maybe String
} deriving (Eq, Show, Generic)
instance FromJSON Message

data Chat = Chat {
    chat_id :: Integer
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
