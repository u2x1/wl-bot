{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE OverloadedStrings #-}
module Plugin.NHentaiQuerier where

import qualified Data.Text as Text
import           Data.Aeson
import           Utils.Json
import           Utils.Misc as Misc
import Core.Type.Unity.Request
import Core.Type.Unity.Update
import Control.Monad.Trans.Maybe
import Core.Data.Unity
import           GHC.Generics
import Control.Monad
import           Utils.Logging
import           Network.Wreq as Wreq
import Control.Lens
import Control.Monad
import  Data.List

type Title = Text.Text
type Id    = Text.Text

getNHentaiBookId :: Title -> MaybeT IO (Title, Id)
getNHentaiBookId bName = do
  let opts = defaults & param "query" .~ [bName]
  r <- liftM $ Wreq.getWith opts $ "https://nhentai.net/api/galleries/search"
  case eitherDecode $ r ^. responseBody of
    Right nr -> do
      logWT Debug "1"
      case nh_result nr of
        Just results -> do
          logWT Debug "2"
          case find (\x -> elem "chinese" (map (nh_tag_name) (nh_tags x))) results of
            Just finalResult -> pure . Just $ (getId$nh_id finalResult, nh_title_japanese$nh_title finalResult)
            _ -> pure . Just $ (getId$nh_id (head results), nh_title_japanese $nh_title (head results))
        _ -> pure $ Nothing
    Left x -> logWT Debug x >> (pure $ Nothing)

getId :: NHWTFId -> Text.Text
getId (NHWTFId i) = i

processNHentaiQuery :: (Text.Text, Update) -> IO [SendMsg]
processNHentaiQuery (cmdBody, update) =
  if content /= ""
     then do
       logWT Info $
         "NHentai query: [" <> Text.unpack content <> "] sending from " <> show (user_id update)
       r <- getNHentaiBookId $ content
       case r of
         Just result -> let msg = Misc.unlines ["[标题] " <> fst result, "[链接] " <> snd result] in
                          pure [makeReqFromUpdate update msg]
         Nothing -> pure [makeReqFromUpdate update "无结果。"]
     else pure []
  where
    content = Text.strip cmdBody

data NHentaiResults = NHentaiResults {
    nh_result :: Maybe [NHentaiResult]
} deriving (Show, Generic)
instance FromJSON NHentaiResults where
  parseJSON = dropParseJSON 3

-- NHentai WTF Id: The id field of the JSON data fetching from NHentai may have type Int or String.
data NHWTFId = NHWTFId Text.Text
  deriving (Show)
instance FromJSON NHWTFId where
  parseJSON (Number o) = pure $ NHWTFId $ (fst . Text.breakOn ".") $ Text.pack $ show o
  parseJSON (String o) = pure $ NHWTFId o
  parseJSON _ = mzero

data NHentaiResult = NHentaiResult {
    nh_id :: NHWTFId
  , nh_title :: NHentaiResultTitle
  , nh_tags :: [NHentaiResultTag]
} deriving (Show, Generic)
instance FromJSON NHentaiResult where
  parseJSON = dropParseJSON 3

data NHentaiResultTitle = NHentaiResultTitle {
    nh_title_english :: Text.Text
  , nh_title_japanese :: Text.Text
} deriving (Show, Generic)
instance FromJSON NHentaiResultTitle where
  parseJSON = dropParseJSON 9

data NHentaiResultTag = NHentaiResultTag {
    nh_tag_name :: Text.Text
} deriving (Show, Generic)
instance FromJSON NHentaiResultTag where
  parseJSON = dropParseJSON 7
