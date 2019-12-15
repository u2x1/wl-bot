{-# LANGUAGE DeriveGeneric #-}
module Type.Config where

import Data.Aeson
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

-- data GroupMap = GroupMap {
--     groupMap :: [(Int, Int)]
-- } deriving (Eq, Show, Generic)
-- instance FromJSON GroupMap
-- instance ToJSON GroupMap
