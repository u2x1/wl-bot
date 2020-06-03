{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Core.Type.Mirai.Update where

import Data.Aeson
import GHC.Generics
import Utils.Json

data Update = Update {
    mirai_type :: String
  , mirai_message_id :: Integer
  , mirai_reply_id :: Maybe Integer
  , mirai_message_chain :: [MRMsg]
  , mirai_sender :: MRSender
} deriving (Show, Generic)
instance FromJSON Update where
  parseJSON = withObject "Update" $ \v -> Update
    <$> (v .: "type")
    <*> ((v .: "messageChain") >>= (parseJSON.head) >>= (.: "id"))
    <*> ((v .: "messageChain") >>=
      (\m -> if length m < 2 then pure Nothing else parseJSON (m !! 1) >>= (.:? "id")))
    <*> (v .: "messageChain")
    <*> (v .: "sender")

data MRMsg = MRMsg {
    mrm_type   :: String
  , mrm_text   :: Maybe String
  , mrm_url    :: Maybe String
  , mrm_origin :: Maybe [MRMsg]
} deriving (Show, Generic)
instance FromJSON MRMsg where
  parseJSON = dropParseJSON 4
instance ToJSON MRMsg where
  toJSON = dropToJSON 4

data MRSender = MRSender {
    mrs_id :: Integer
  , mrs_group_id :: Maybe Integer
} deriving (Eq, Show, Generic)
instance FromJSON MRSender where
  parseJSON = withObject "MRSender" $ \v -> MRSender
    <$> (v .: "id")
    <*> ((v .:? "group") >>= traverse (.: "id"))
