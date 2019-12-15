{-# LANGUAGE DeriveGeneric #-}
module Type.CQ.SendMsg (SendMsg (SendMsg)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics

data SendMsg = SendMsg {
    group_id :: Integer
  , message :: Text
} deriving (Eq, Show, Generic)

instance FromJSON SendMsg
instance ToJSON SendMsg
