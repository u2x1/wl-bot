{-# LANGUAGE DeriveGeneric #-}
module Utils.Config where

import Data.Aeson
import GHC.Generics

data Config = Config {
--    admins     :: [Integer]
    port       :: Int
  , thisServer :: String
  , cqServer   :: String
  , tgbotToken :: String
  , ws_host    :: String
  , ws_port    :: Int
} deriving (Eq, Show, Generic)

instance FromJSON Config
