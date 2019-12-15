{-# LANGUAGE DeriveGeneric #-}

module Type.CQ.User where

import Data.Text
import Data.Aeson
import GHC.Generics

data User = User {
    user_id :: Integer
  , nickname :: Text
} deriving (Eq, Show, Generic)
instance FromJSON User
