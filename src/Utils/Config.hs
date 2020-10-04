{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Utils.Config where

import           Control.Lens
import           Data.Aeson

data Config = Config {
--    admins     :: [Integer]
    _port              :: Int
  , _webhook_server    :: String
  , _mirai_server      :: String
  , _tg_token          :: String
  , _ws_host           :: String
  , _ws_port           :: Int
  , _mirai_auth_key    :: String
  , _mirai_qq_id       :: String
  , _mirai_session_key :: String
} deriving (Show)
makeLenses ''Config
instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> (v .: "port")
    <*> (v .: "webhook_server")
    <*> (v .: "mirai_server")
    <*> (v .: "tg_token")
    <*> (v .: "ws_host")
    <*> (v .: "ws_port")
    <*> (v .: "mirai_auth_key")
    <*> (v .: "mirai_qq_id")
    <*> pure ""
