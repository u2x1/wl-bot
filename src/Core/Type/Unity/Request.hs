{-# LANGUAGE TemplateHaskell #-}
module Core.Type.Unity.Request where

import Data.Text
import Control.Lens

import Core.Type.Universal

data SendMsg = SendMsg {
    _chat_id     :: Integer
  , _user_id     :: Integer
  , _target_type :: TargetType
  , _target_plat :: Platform
  , _reply_id    :: Maybe Integer
  , _imgUrl      :: Maybe Text
  , _imgPath     :: Maybe String
  , _text        :: Maybe Text
} deriving (Show)
makeLenses ''SendMsg
