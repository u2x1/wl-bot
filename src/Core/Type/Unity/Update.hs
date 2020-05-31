module Core.Type.Unity.Update where

import Core.Type.Universal

data Update = Update {
    platform           :: Platform
  , user_id            :: Integer
  , chat_id            :: Integer
  , message_text       :: Maybe String
  , message_image_urls :: Maybe [String]
  , message_type       :: TargetType
  , message_id         :: Integer
  , reply_id           :: Maybe Integer
} deriving (Show)
