module Core.Type.Universal where

data MsgType = Private | Group
  deriving (Show)

data Platform = Telegram | QQ
  deriving (Show)
