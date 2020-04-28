{-# LANGUAGE OverloadedStrings #-}
module Module.BaikeQuerier where

import           Core.Type.Unity.Update       as UU
import           Core.Type.Unity.Request      as UR (SendMsg)
import           Core.Data.Unity

import           Utils.Logging
import           Utils.Misc                   as Misc

import           Network.Wreq
import           Control.Lens
import qualified Data.ByteString.Lazy.UTF8    as UTF8  (toString)
import qualified Data.ByteString.Lazy         as BL
import           Data.ByteString.Lazy.Search  as BL    (breakOn, breakAfter)
import qualified Data.Text                    as Text
import           Data.Text                             (strip, Text, unpack)
import           Data.Text.Lazy                        (toStrict)
import           Data.Text.Lazy.Encoding
import           Data.List                             (intersperse)
import           Core.Type.EitherT

getWords :: BL.ByteString -> [Text]
getWords "" = []
getWords str = (strip.toStrict.decodeUtf8 $ fst (breakOn "<" xs)) : getWords xs
  where xs = snd $ breakAfter ">" str

-- Select fragments that are not equal to "&nbsp;" or started with "\n"
concatWord :: [Text] -> Text
concatWord oStr = (mconcat.intersperse "\n\n") s
  where s = foldr addNextLine [] $
              filter
              (\str -> str /= "&nbsp;" && str /= "" && Text.head str /= '[')
              oStr
        addNextLine x [] = [x]
        addNextLine x xs = let a = strip x in
                            if Text.last a == '。'
                               then a : xs
                               else (a <> head xs) : tail xs

runBaiduSearch :: Text -> IO Text
runBaiduSearch query = do
  x <- runMEitherT $ do
    realUrl <- liftMaybe "未找到词条。" (getFstUrl <$> getWith opts "https://www.baidu.com/s")
    realRst <- liftMaybe ("词条无摘要:\n" <> Text.pack realUrl) (getFirstPara <$> get realUrl)
    pure $ concatWord.getWords $ realRst
  pure $ getTextT x
  where
    getFirstPara x = searchBetweenBL
                       "<div class=\"lemma-summary\" label-module=\"lemmaSummary\""
                       "lemmaWgt"
                       (x ^. responseBody)
    getFstUrl content = ("https://baike.baidu.com/item" <>).UTF8.toString <$> searchBetweenBL "baike.baidu.com/item" "\"" (BL.drop 180000 (content ^. responseBody))
    opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.1; WOW64; rv:34.0) Gecko/20100101 Firefox/73.0"]
                    & param "wd" .~ [query <> " site:baike.baidu.com"]
                    & param "ie" .~ ["utf-8"] & param "pn" .~ ["0"]
                    & param "cl" .~ ["3"] & param "rn" .~ ["100"]

processBaiduQuery :: (Text, Update) -> IO [SendMsg]
processBaiduQuery (cmdBody, update) =
  if cmdBody /= ""
     then do
       result <- runBaiduSearch cmdBody
       logWT Info $
         "Query: [" <> unpack cmdBody <> "] sending from " <> show (user_id update)
       pure [makeReqFromUpdate update result]
     else pure []
