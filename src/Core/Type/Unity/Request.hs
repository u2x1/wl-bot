module Core.Type.Unity.Request where

import Data.Text

import Core.Type.Universal

data SendMsg = SendMsg {
    chat_id :: Integer
  , msg_type :: MsgType
  , target_plat :: Platform
  , text :: Text
} deriving (Show)
