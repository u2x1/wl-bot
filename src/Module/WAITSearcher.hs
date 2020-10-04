{-# LANGUAGE OverloadedStrings #-}
module Module.WAITSearcher where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Network.Wreq
import           Utils.Logging

import           Control.Exception
import           Core.Data.Unity
import           Core.Type.Unity.Request     (SendMsg)
import           Core.Type.Unity.Update
import           Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as Text
import           Utils.Misc                  as Misc

runWAITSearch :: String -> IO (Either String WAITResults)
runWAITSearch imgUrl' = do
  img <- get imgUrl'
  let imgBase64 = Base64.encode $ img ^. responseBody
  r <- try $ post "https://trace.moe/api/search" ["image" := imgBase64]
                :: IO (Either SomeException (Response BL.ByteString))
  case r of
    Right realR -> case eitherDecode $ realR ^. responseBody of
                     Right x  -> (pure.Right) x
                     Left str -> (pure.Left) $ "未找到近似图片。"<> str
    Left excp -> (pure.Left) $
           "请求错误: " <> show excp

processWAITQuery :: (Text.Text, Update) -> IO [SendMsg]
processWAITQuery (_, update) = let imgUrls = message_image_urls update in
  if null imgUrls
    then pure [makeReqFromUpdate update "无效图片。"]
    else do
       result <- runWAITSearch $ head imgUrls
       logWT Info $
         "WAIT [" <> head imgUrls <> "] sending from " <> show (user_id update)
       either' result (\x -> pure [makeReqFromUpdate update $ Text.pack x]) (\rst ->
         pure [makeReqFromUpdate update . Misc.unlines $
           let fstRst = head $ wt_docs rst
               similarity = show $ wt_similarity fstRst
               anime = wt_anime fstRst
               season = wt_season fstRst
               episode = show $ wt_episode fstRst
               atTime = wt_at fstRst in
           [ "[相似度] " <> Text.pack similarity
           , "[动漫] " <> anime
           , "[季度] " <> season
           , "[位置] #" <> Text.pack episode <> " " <> toTime atTime]])
        where
          toTime x = Text.pack $ show (div (round x) 60 :: Int) <> ":" <> show (mod (round x) 60 :: Int)

newtype WAITResults = WAITResults {
    wt_docs :: [WAITResult]
} deriving (Show)
instance FromJSON WAITResults where
  parseJSON = withObject "WAITResults" $ \v -> WAITResults
    <$> (v .: "docs")

data WAITResult = WAITResult {
    wt_similarity :: Float
  , wt_anime      :: Text.Text
  , wt_season     :: Text.Text
  , wt_episode    :: Text.Text
  , wt_at         :: Float
} deriving (Show)
instance FromJSON WAITResult where
  parseJSON = withObject "WAITResults" $ \v -> WAITResult
    <$> ((*100) <$> v .: "similarity")
    <*> (v .: "anime")
    <*> (v .: "season")
    <*> ((v .: "episode") >>= parseEpisode)
    <*> (v .: "at")

parseEpisode :: Value -> Parser Text.Text
parseEpisode (Number o) = pure $ (fst . Text.breakOn ".") $ Text.pack $ show o
parseEpisode (String o) = pure o
parseEpisode _          = mzero
