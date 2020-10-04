module Core.Type.Universal where

data MsgType = Image | Text
  deriving (Show)

data TargetType = Private | Group | Temp
  deriving (Eq, Show)

data Platform = Telegram | QQ
  deriving (Eq, Show)
