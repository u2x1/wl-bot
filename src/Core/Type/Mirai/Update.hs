{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Core.Type.Mirai.Update where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Utils.Json

data Update = Update {
    mirai_type :: String
  , mirai_message_id :: Integer
  , mirai_message_chain :: [MRMsg]
  , mirai_sender :: MRSender
} deriving (Show, Generic)
instance FromJSON Update where
  parseJSON = withObject "Update" $ \v -> Update
    <$> (v .: "type")
    <*> ((v .: "messageChain") >>= (parseJSON.head) >>= (.: "id"))
    <*> (v .: "messageChain")
    <*> (v .: "sender")

data MRMsg = MRMsg {
    mrm_type :: Text
  , mrm_text :: Maybe Text
  , mrm_url  :: Maybe Text
} deriving (Show, Generic)
instance FromJSON MRMsg where
  parseJSON = dropParseJSON 4

data MRSender = MRSender {
    mrs_id :: Integer
  , mrs_group_id :: Maybe Integer
} deriving (Eq, Show, Generic)
instance FromJSON MRSender where
  parseJSON = withObject "MRSender" $ \v -> MRSender
    <$> (v .: "id")
    <*> ((v .:? "group") >>= (traverse (.: "id")))
