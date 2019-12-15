{-# LANGUAGE DeriveGeneric #-}
module Type.Telegram.Update where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics

data Update = Update {
    update_id :: Integer
  , message :: Message
} deriving (Eq, Show, Generic)
instance FromJSON Update
instance ToJSON Update

data Message = Message {
    message_id :: Integer
  , from :: User
  , chat :: Chat
  , text :: Maybe Text
} deriving (Eq, Show, Generic)
instance FromJSON Message
instance ToJSON Message

data Chat = Chat {
    id :: Integer
  , username :: Text
} deriving (Eq, Show, Generic)
instance FromJSON Chat
instance ToJSON Chat

data User = User {
    first_name :: Text
  , last_name :: Maybe Text
} deriving (Eq, Show, Generic)
instance FromJSON User
instance ToJSON User
