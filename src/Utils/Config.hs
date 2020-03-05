{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Utils.Config where

import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy as BL
import Utils.Logging
import Data.Text (Text)
import GHC.Generics

type GroupMap = [(Integer, Integer)]

data Config = Config {
--    admins :: [Int]
    port :: Int
  , cqServer :: String
  , tgbotToken :: String
  , groups :: GroupMap
} deriving (Eq, Show, Generic)
instance FromJSON Config
instance ToJSON Config

exampleConfig = Config 
                  8443
                  "http://localhost:5700"
                  "telegramtokenhere"
                  [(12312313, - 45412346)]

makeExampleConfig :: SomeException -> IO ByteString
makeExampleConfig _ = do
  logWT "Error" "No config file found. Creating example file."
  BL.writeFile "config.json" (encode exampleConfig)
  return (encode exampleConfig)

