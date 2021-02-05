{-# LANGUAGE TemplateHaskell #-}
module Core.Type.Unity.Request where

import           Control.Lens
import           Data.Text

import           Core.Type.Universal

data SendMsg = SendMsg
  { _chat_id     :: String
  , _user_id     :: String
  , _target_type :: TargetType
  , _target_plat :: Platform
  , _reply_id    :: Maybe String
  , _imgUrl      :: Maybe Text
  , _imgPath     :: Maybe String
  , _text        :: Maybe Text
  }
  deriving Show
makeLenses ''SendMsg
