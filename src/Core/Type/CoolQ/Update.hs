{-# LANGUAGE DeriveGeneric #-}
module Core.Type.CoolQ.Update where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Utils.Json

data Update = Update {
    post_type :: Text
  , message_id :: Maybe Integer
  , message_type :: String
  , sender :: Maybe Sender
  , group_id :: Maybe Integer
  , user_id :: Maybe Integer
  , message :: [CQMsg]
  , raw_message :: Text
} deriving (Eq, Show, Generic)
instance FromJSON Update

data CQMsg = CQMsg {
    cqtype :: Text
  , cqdata :: CQMsgData
} deriving (Eq, Show, Generic)
instance FromJSON CQMsg where
  parseJSON = dropParseJSON 2

data CQMsgData = CQMsgData {
    text :: Maybe Text
  , id :: Maybe Text
  , file :: Maybe Text
  , url :: Maybe Text
} deriving (Eq, Show, Generic)
instance FromJSON CQMsgData

newtype Sender = Sender {
    nickname :: Text
} deriving (Eq, Show, Generic)
instance FromJSON Sender
