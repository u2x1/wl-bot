{-# LANGUAGE OverloadedStrings #-}
module Plugin.SauceNAOSearcher
--  (runSauceNAOSearch, processSnaoQuery, snaoHelps)
where

import           Network.Wreq as Wreq
import           Control.Lens
import qualified Data.Text as Text
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Control.Exception
import           Core.Type.Unity.Update  as UU
import           Core.Type.Unity.Request as UR
import           Utils.Logging
import           Utils.Misc as Misc
import           Core.Data.Unity

import           Plugin.NHentaiQuerier

type Similarity = Text.Text
type Url        = Text.Text
runSauceNAOSearch :: String -> Text.Text -> IO (Either String SnaoResult)
runSauceNAOSearch apiKey imgUrl = do
  r <- try.Wreq.get $
        snaoHost <> "?db=999&output_type=2&numres=1&api_key=" <> apiKey <> "&url=" <> Text.unpack imgUrl :: IO (Either SomeException (Response BL.ByteString))
  case r of
    Right realR -> case eitherDecode $ realR ^. responseBody of
                     Right x -> (pure.Right) x
                     Left str -> (pure.Left) $ "未找到近似图片。"<>str
    Left excp ->
      if snd (Text.breakOn "rate limit" (Text.pack $ show excp)) == ""
         then (pure.Left) $ "请求错误: " <> show excp
         else (pure.Left) "已超出30秒内搜图限制。"
    where snaoHost = "https://saucenao.com/search.php"

processSnaoQuery :: (Text.Text, Update) -> IO [SendMsg]
processSnaoQuery (_, update) =
   case message_image_urls update of
     Just imgUrls -> do
       result <- runSauceNAOSearch "d4c5f40172cb923c73c409538f979482a469d5a7" $ head imgUrls
       logWT Info $
         "SauceNAO Query: [" <> Text.unpack (head imgUrls) <> "] sending from " <> show (user_id update)
       case result of
         Right rst ->
           case (sr_ext_url.head.sr_results) rst of
             Just extUrls ->
               pure [makeReqFromUpdate update $ Misc.unlines
                  [ "[相似度] " <> (sr_similarity.head.sr_results) rst
                  , "[缩略图] " <> (sr_thumbnail.head.sr_results) rst
                  , "[图源] "   <> head extUrls]]
             _ -> case (sr_doujinshi_name.head.sr_results) rst of
                    Just dn -> do
                      n <- getNHentaiBookId dn
                      let nh = case n of
                                  Just nhentaiInfo -> 
                                    [[ "<NHentai搜索结果>"
                                    , "[标题] " <> snd nhentaiInfo
                                    , "" <> "https://nhentai.net/g/" <> fst nhentaiInfo]]
                                  _ -> [[]]
                      pure $ makeReqFromUpdate update.Misc.unlines <$>
                          [ "[相似度] " <> (sr_similarity.head.sr_results) rst
                          , "[缩略图] " <> (sr_thumbnail.head.sr_results) rst
                          , "[本子名] " <> dn] : nh
                        
                    _ -> pure []
         Left x -> pure [makeReqFromUpdate update $ Text.pack x]
     _ -> pure []

snaoHelps :: [Text.Text]
snaoHelps = ["{sp<PIC>} 从saucenao.net搜索一张图片。"]

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
  , sr_doujinshi_name :: Maybe Text.Text
} deriving (Show)
instance FromJSON SnaoResults where
  parseJSON = withObject "SnaoResults" $ \v -> SnaoResults
        <$> ((v .: "header") >>= (.: "similarity"))
        <*> ((v .: "header") >>= (.: "thumbnail"))
        <*> ((v .: "data") >>= (.:? "ext_urls"))
        <*> ((v .: "data") >>= (.:? "jp_name"))
