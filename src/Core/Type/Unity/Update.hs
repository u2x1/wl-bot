module Core.Type.Unity.Update where

import Data.Text
import Core.Type.Universal

data Update = Update {
    platform      :: Platform
  , user_id       :: Integer
  , user_nickname :: Text
  , chat_id       :: Integer
  , message_text  :: Text
  , message_type :: MsgType
  , message_id    :: Integer
--  , group_id      :: Maybe Integer
} deriving (Show)
