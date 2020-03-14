module Core.Type.Unity.Update where

import Data.Text

data Update = Update {
    user_id       :: Integer
  , message_id    :: Integer
  , chat_id       :: Integer
  , group_id      :: Maybe Integer
  , user_nickname :: Text
  , message_text  :: Text
}
