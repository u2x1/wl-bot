{-# LANGUAGE DeriveGeneric #-}
module Utils.Config where

import Data.Aeson
import GHC.Generics

data Config = Config {
    admins     :: [Integer]
  , port       :: Int
  , thisServer :: String
  , cqServer   :: String
  , tgbotToken :: String
} deriving (Eq, Show, Generic)

instance FromJSON Config
