module Core.Type.Unity.Request where

import Data.Text

import Core.Type.Universal

data SendMsg = SendMsg {
    text :: Text
  , chat_id :: Integer
  , msg_type :: MsgType
  , target_plat :: Platform
}
