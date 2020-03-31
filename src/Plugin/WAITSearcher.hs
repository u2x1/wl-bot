{-# LANGUAGE OverloadedStrings #-}
module Plugin.WAITSearcher where

import Network.Wreq
import Data.Aeson
import Control.Lens
import Utils.Logging

import qualified Data.Text as Text
import Core.Data.Unity
import Core.Type.Unity.Request
import Core.Type.Unity.Update
import Utils.Misc as Misc
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Base64.Lazy as Base64

runWAITSearch :: Text.Text -> IO (Either String WAITResults)
runWAITSearch imgUrl = do
  img <- get $ Text.unpack imgUrl
  let imgBase64 = Base64.encode $ img ^. responseBody
  r <- try $ post "https://trace.moe/api/search" ["image" := imgBase64]
                :: IO (Either SomeException (Response BL.ByteString))
  case r of
    Right realR -> case eitherDecode $ realR ^. responseBody of
                     Right x -> (pure.Right) x
                     Left str -> (pure.Left) $ "未找到近似图片。"<> str
    Left excp -> (pure.Left) $
           "请求错误: " <> show excp

processWAITQuery :: (Text.Text, Update) -> IO [SendMsg]
processWAITQuery (_, update) =
   maybe' (message_image_urls update) (pure [makeReqFromUpdate update "无效图片。"]) (\imgUrls -> do
       result <- runWAITSearch $ head imgUrls
       logWT Info $
         "WAIT [" <> Text.unpack (head imgUrls) <> "] sending from " <> show (user_id update)
       either' result (\x -> pure [makeReqFromUpdate update $ Text.pack x]) (\rst ->
         pure $ [(makeReqFromUpdate update) . Misc.unlines $
           let fstRst = head $ wt_docs rst
               similarity = show $ wt_similarity fstRst
               anime = wt_anime fstRst
               season = wt_season fstRst
               episode = show $ wt_episode fstRst
               atTime = wt_at fstRst in
           [ "[相似度] " <> Text.pack similarity
           , "[动漫] " <> anime
           , "[季度] " <> season
           , "[位置] #" <> Text.pack episode <> " " <> toTime atTime]]))
        where
          toTime x = Text.pack $ (show $ div (round x) 60) <> ":" <> (show $ mod (round x) 60)

data WAITResults = WAITResults {
    wt_docs :: [WAITResult]
} deriving (Show)
instance FromJSON WAITResults where
  parseJSON = withObject "WAITResults" $ \v -> WAITResults
    <$> (v .: "docs")

data WAITResult = WAITResult {
    wt_similarity :: Float
  , wt_anime :: Text.Text
  , wt_season :: Text.Text
  , wt_episode :: Int
  , wt_at :: Float
} deriving (Show)
instance FromJSON WAITResult where
  parseJSON = withObject "WAITResults" $ \v -> WAITResult
    <$> ((*100) <$> v .: "similarity")
    <*> (v .: "anime")
    <*> (v .: "season")
    <*> (v .: "episode")
    <*> (v .: "at")
