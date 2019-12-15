{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.CQ.Update where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data Update = Update {
    post_type :: Text
  , message_type :: Text
  , sender :: Sender
  , group_id :: Maybe Integer
  , message :: [CQMsg]
  , raw_message :: Maybe Text
} deriving (Eq, Show, Generic)
instance FromJSON Update

data CQMsg = CQMsg {
    cqtype :: Text
  , cqdata :: CQMsgData
} deriving (Eq, Show)
instance FromJSON CQMsg where
  parseJSON = withObject "CQMsg" $ \v -> CQMsg
    <$> v .: "type"
    <*> v .: "data"

data CQMsgData = CQMsgData {
    text :: Maybe Text
  , id :: Maybe Text
  , file :: Maybe Text
  , url :: Maybe Text
} deriving (Eq, Show, Generic)
instance FromJSON CQMsgData

data Sender = Sender {
    nickname :: Text
} deriving (Eq, Show, Generic)
instance FromJSON Sender
