module Core.Type.Unity.Update where

import Data.Text
import Core.Type.Universal

data Update = Update {
    platform           :: Platform
  , user_id            :: Integer
  , chat_id            :: Integer
  , message_text       :: Maybe Text
  , message_image_urls :: Maybe [Text]
  , message_type       :: TargetType
  , message_id         :: Integer
} deriving (Show)
