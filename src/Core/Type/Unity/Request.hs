module Core.Type.Unity.Request where

import Data.Text

import Core.Type.Universal

data SendMsg = SendMsg {
    chat_id :: Integer
  , user_id :: Integer
  , target_type :: TargetType
  , target_plat :: Platform
  , reply_id :: Maybe Integer
  , imgUrl :: Maybe Text
  , imgPath :: Maybe String
  , text :: Maybe Text
} deriving (Show)
