module Core.Type.Unity.Request where

import Data.Text

import Core.Type.Universal

data SendMsg = SendMsg {
    chat_id :: Integer
  , target_type :: TargetType
  , target_plat :: Platform
  , reply_id :: Maybe Integer
  , imgUrls :: Maybe [Text]
  , imgPath :: Maybe Text
  , text :: Maybe Text
} deriving (Show)
