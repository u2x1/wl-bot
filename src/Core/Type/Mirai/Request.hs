{-# LANGUAGE DeriveGeneric #-}
module Core.Type.Mirai.Request where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

import Utils.Json

data SendMRMsg = SendMRMsg {
    mr_qq              :: Maybe Integer
  , mr_group           :: Maybe Integer
  , mr_sessionKey      :: String
  , mr_quote           :: Maybe Integer
  , mr_messageChain    :: [Message]
} deriving (Eq, Show, Generic)
instance ToJSON SendMRMsg where
  toJSON = dropToJSON 3

data Message = Message {
    mrc_type :: Text
  , mrc_text :: Maybe Text
  , mrc_url  :: Maybe Text
  , mrc_path :: Maybe Text
} deriving (Eq, Show, Generic)
instance ToJSON Message where
  toJSON = dropToJSON 4
