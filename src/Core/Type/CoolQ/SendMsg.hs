{-# LANGUAGE DeriveGeneric #-}
module Core.Type.CoolQ.SendMsg where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

import Utils.Json

data SendGrpMsg = SendGrpMsg {
    grp_group_id :: Integer
  , grp_message :: Text
} deriving (Eq, Show, Generic)
instance ToJSON SendGrpMsg where
  toJSON = dropToJSON 4

data SendPrivMsg = SendPrivMsg {
    priv_user_id :: Integer
  , priv_message :: Text
} deriving (Eq, Show, Generic)
instance ToJSON SendPrivMsg where
  toJSON = dropToJSON 5
