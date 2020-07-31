{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Utils.ModuleHelper where

import Data.Aeson
import GHC.Generics
import Network.Wreq as Wreq
import Control.Lens


import Data.Text (Text)

getShortUrl :: Text -> IO (Maybe Text)
getShortUrl originUrl = do
  rsp <- Wreq.post "https://29.pm/api.php" ["d" := originUrl]
  let s = decode (rsp ^. responseBody) :: Maybe ShortURL
  return (shorturl <$> s)

newtype ShortURL = ShortURL {
   shorturl :: Text
} deriving (Generic, Show)
instance FromJSON ShortURL