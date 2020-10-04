{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Module.NHentaiQuerier where

import           Control.Lens
import           Control.Monad
import           Core.Data.Unity
import           Core.Type.Unity.Request (SendMsg)
import           Core.Type.Unity.Update
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable
import qualified Data.Text               as Text
import           GHC.Generics
import           Network.Wreq            as Wreq
import           Utils.Json
import           Utils.Logging
import           Utils.Misc              as Misc

type Title = Text.Text
type Id    = Text.Text

getNHentaiBookId :: Title -> IO (Maybe (Title, Id))
getNHentaiBookId bName = do
  let opts = defaults & param "query" .~ [bName]
  r <- Wreq.getWith opts "https://nhentai.net/api/galleries/search"
  pure $
    either' (eitherDecode $ r ^. responseBody) (\e -> Just (Text.pack e,"")) (\nh ->
      maybe' (nh_result nh) Nothing (\results ->
        if null results
           then Nothing
           else Just $ maybe' (find (elem "chinese" . nh_tags) results) (getInfo $ head results) getInfo))
  where
    getInfo x = (,) (nh_title x) (nh_id x)

processNHentaiQuery :: (Text.Text, Update) -> IO [SendMsg]
processNHentaiQuery (cmdBody, update) =
  if content /= ""
     then do
       logWT Info $
         "NHentai query: [" <> Text.unpack content <> "] sending from " <> show (user_id update)
       r <- getNHentaiBookId content
       case r of
         Just result -> let msg = Misc.unlines ["[标题] " <> fst result
                                              , "[链接] https://nhentai.net/g/" <> snd result] in
                          pure [makeReqFromUpdate update msg]
         Nothing -> pure [makeReqFromUpdate update "无结果。"]
     else pure []
  where
    content = Text.strip cmdBody

newtype NHentaiResults = NHentaiResults {
    nh_result :: Maybe [NHentaiResult]
} deriving (Show, Generic)
instance FromJSON NHentaiResults where
  parseJSON = dropParseJSON 3

data NHentaiResult = NHentaiResult {
    nh_id    :: Text.Text
  , nh_title :: Text.Text
  , nh_tags  :: [Text.Text]
} deriving (Show)
instance FromJSON NHentaiResult where
  parseJSON = withObject "NHentaiResult" $ \v -> NHentaiResult
    <$> ((v .: "id") >>= parseId)
    <*> ((v .: "title") >>= (.: "japanese"))
    <*> ((v .: "tags" :: Parser Array) >>=
      traverse parseJSON . toList >>=
        traverse (.: "name") )

parseId :: Value -> Parser Text.Text
parseId (Number o) = pure $ (fst . Text.breakOn ".") $ Text.pack $ show o
parseId (String o) = pure o
parseId _          = mzero
