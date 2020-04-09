{-# LANGUAGE OverloadedStrings #-}
module Module.SauceNAOSearcher where

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

import           Module.NHentaiQuerier

type Similarity = Text.Text
type Url        = Text.Text
runSauceNAOSearch :: String -> String -> IO (Either String SnaoResults)
runSauceNAOSearch apiKey imgUrl = do
  let opts = defaults & param "db" .~ ["999"]
                      & param "output_type" .~ ["2"]
                      & param "numres" .~ ["1"]
                      & param "api_key" .~ [Text.pack apiKey]
                      & param "url" .~ [Text.pack imgUrl]
  r <- try $ Wreq.getWith opts "https://saucenao.com/search.php"
                :: IO (Either SomeException (Response BL.ByteString))
  case r of
    Right realR -> case eitherDecode $ realR ^. responseBody of
                     Right x -> (pure.Right) x
                     Left str -> (pure.Left) $ "未找到近似图片。"<> str
    Left excp -> (pure.Left) $
      if snd (Text.breakOn "rate limit" (Text.pack $ show excp)) == ""
         then  "请求错误: " <> show excp
         else  "已超出30秒内搜图限制。"

processSnaoQuery :: (Text.Text, Update) -> IO [SendMsg]
processSnaoQuery (_, update) =
   maybe' (message_image_urls update) (pure [makeReqFromUpdate update "无效图片。"]) (\imgUrls' -> do
       result <- runSauceNAOSearch "d4c5f40172cb923c73c409538f979482a469d5a7" $ head imgUrls'
       logWT Info $
         "SauceNAO [" <> head imgUrls' <> "] sending from " <> show (user_id update)
       either' result (\x -> pure [makeReqFromUpdate update $ Text.pack x]) (\rst ->
           let fstRst = head $ sr_results rst in
           case sr_ext_url fstRst of
             Just extUrls -> pure [makeReqFromUpdate' update (Just [sr_thumbnail fstRst]) $ Just $ Misc.unlines
                                                        [ "[相似度] " <> sr_similarity fstRst
                                                        , "[图源] " <> head extUrls]]
             _ -> (:[]) <$> (makeReqFromUpdate update) . Misc.unlines <$>  maybe' (sr_doujinshi_name fstRst) (pure []) (\dn -> do
                   n <- getNHentaiBookId dn
                   pure $ ["[本子名] " <> dn] <>
                     maybe' n [] (\info ->
                          ["[链接]" <> "https://nhentai.net/g/" <> snd info]))))

data SnaoResults = SnaoResults {
    sh_short_remaining :: Int
  , sh_long_remaining :: Int
  , sh_status :: Int
  , sr_results :: [SnaoResult]
} deriving (Show)
instance FromJSON SnaoResults where
  parseJSON = withObject "SnaoResults" $ \v -> SnaoResults
        <$> ((v .: "header") >>= (.: "short_remaining"))
        <*> ((v .: "header") >>= (.: "long_remaining"))
        <*> ((v .: "header") >>= (.: "status"))
        <*> (v .: "results")

data SnaoResult = SnaoResult {
    sr_similarity :: Text.Text
  , sr_thumbnail :: Text.Text
  , sr_ext_url :: Maybe [Text.Text]
  , sr_doujinshi_name :: Maybe Text.Text
} deriving (Show)
instance FromJSON SnaoResult where
  parseJSON = withObject "SnaoResult" $ \v -> SnaoResult
        <$> ((v .: "header") >>= (.: "similarity"))
        <*> ((v .: "header") >>= (.: "thumbnail"))
        <*> ((v .: "data") >>= (.:? "ext_urls"))
        <*> ((v .: "data") >>= (.:? "jp_name"))
