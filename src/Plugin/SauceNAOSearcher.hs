{-# LANGUAGE OverloadedStrings #-}
module Plugin.SauceNAOSearcher
--  (runSauceNAOSearch, processSnaoQuery, snaoHelps)
where

import           Network.Wreq as Wreq
import           Control.Lens
import qualified Data.Text as Text
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Control.Exception
import           Core.Type.Unity.Update  as UU
import           Core.Type.Unity.Request as UR
import           Utils.Logging
import           Core.Data.Unity

type Similarity = Text.Text
type Url        = Text.Text
runSauceNAOSearch :: String -> Text.Text -> IO (Either String SnaoResult)
runSauceNAOSearch apiKey imgUrl = do
  r <- (try $ Wreq.get $
                  snaoHost <> "?db=999&output_type=2&numres=1&api_key=" <> apiKey <> "&url=" <> Text.unpack imgUrl) :: IO (Either SomeException (Response BL.ByteString))
  case r of
    Right realR -> case decode $ realR ^. responseBody of
                     Just x -> (pure.Right) x
                     _ -> (pure.Left) "未找到近似图片。"
    Left excp -> (pure.Left) $ "请求错误: " <> show excp
    where snaoHost = "https://saucenao.com/search.php"

processSnaoQuery :: (Text.Text, Update) -> IO [SendMsg]
processSnaoQuery (_, update) =
   case message_image_urls update of
     Just imgUrls -> do
       result <- runSauceNAOSearch "" $ head imgUrls
       logWT Info $
         "SauceNAO Query: [" <> Text.unpack (head imgUrls) <> "] sending from " <> show (user_id update)
       case result of
         Right rst ->
           case (sr_ext_url.head.sr_results) rst of
             Just extUrls ->
               pure [makeReqFromUpdate update $ Text.unlines
                  [ "[相似度] " <> (sr_similarity.head.sr_results) rst
                  , "[缩略图] " <> (sr_thumbnail.head.sr_results) rst
                  , "[图源] "   <> head extUrls
                  , "[剩余次数] " <> Text.pack (show (sh_short_remaining rst))
                  ]]
             _ -> pure [makeReqFromUpdate update $
                     "该图片没有来源，也许你想看看这个:" <> (sr_thumbnail.head.sr_results) rst]
         Left x -> pure [makeReqFromUpdate update $ Text.pack x]
     _ -> pure [makeReqFromUpdate update snaoHelps]

snaoHelps :: Text.Text
snaoHelps = Text.unlines [ "====SauceNAOSearcher===="
                         , "/sp<PIC>: 从SauceNAO搜索一张图片。"]

data SnaoResult = SnaoResult {
    sh_short_remaining :: Int
  , sh_long_remaining :: Int
  , sh_status :: Int
  , sr_results :: [SnaoResults]
} deriving (Show)
instance FromJSON SnaoResult where
  parseJSON = withObject "SnaoResult" $ \v -> SnaoResult
        <$> ((v .: "header") >>= (.: "short_remaining"))
        <*> ((v .: "header") >>= (.: "long_remaining"))
        <*> ((v .: "header") >>= (.: "status"))
        <*> (v .: "results")

data SnaoResults = SnaoResults {
    sr_similarity :: Text.Text
  , sr_thumbnail :: Text.Text
  , sr_ext_url :: Maybe [Text.Text]
} deriving (Show)
instance FromJSON SnaoResults where
  parseJSON = withObject "SnaoResults" $ \v -> SnaoResults
        <$> ((v .: "header") >>= (.: "similarity"))
        <*> ((v .: "header") >>= (.: "thumbnail"))
        <*> ((v .: "data") >>= (.: "ext_urls"))
