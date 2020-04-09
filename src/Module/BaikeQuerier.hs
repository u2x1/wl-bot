{-# LANGUAGE OverloadedStrings #-}
module Module.BaikeQuerier where

import           Core.Type.Unity.Update       as UU
import           Core.Type.Unity.Request      as UR
import           Core.Data.Unity

import           Utils.Logging
import qualified Utils.Misc as Misc

import           Network.Wreq
import           Control.Lens
import qualified Data.ByteString.Lazy.UTF8    as UTF8  (toString)
import qualified Data.ByteString.Lazy         as BL
import           Data.ByteString.Lazy.Search  as BL    (breakOn, breakAfter)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as TextL
import           Data.Text.Lazy.Encoding
import           Data.List                             (intersperse)

getFstUrl :: BL.ByteString -> Maybe String
getFstUrl content = fixUrl $ UTF8.toString <$> Misc.searchBetweenBL "baike.baidu.com/item" "\"" (BL.drop 180000 content)
  where fixUrl = fmap ("https://baike.baidu.com/item" ++)

getWords :: BL.ByteString -> [Text.Text]
getWords "" = []
getWords str = (Text.strip.TextL.toStrict.decodeUtf8 $ fst (breakOn "<" xs)) : getWords xs
  where xs = snd $ breakAfter ">" str

-- Select fragments that are not equal to "&nbsp;" or started with "\n"
concatWord :: [Text.Text] -> Text.Text
concatWord oStr = (mconcat.intersperse "\n\n") s
  where s = foldr addNextLine [] $
              filter
              (\str -> str /= "" && Text.head str /= '[') $
              Text.replace "&nbsp;" "" <$> oStr
        addNextLine a [] = [a]
        addNextLine a (bss@(b:bs)) = let a' = Text.strip a in
                                         if Text.last a' == ('。')
                                            then a' : bss
                                            else (a' <> b):bs

runBaiduSearch :: Text.Text -> IO Text.Text
runBaiduSearch query = do
  result <- getWith opts $ "https://www.baidu.com/s"
  case getFstUrl (result ^. responseBody) of
    Nothing      -> pure "无结果。"
    Just realUrl -> do
      realRsp <- get realUrl
      case  getFirstPara $ realRsp ^. responseBody of
        Nothing -> pure (Text.pack $
                     "词条无摘要，查看此处:\n" <> realUrl)
        Just resultText -> pure $ concatWord.getWords $ resultText
  where
    getFirstPara = Misc.searchBetweenBL "<div class=\"lemma-summary\" label-module=\"lemmaSummary\"" "lemmaWgt"
    opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.1; WOW64; rv:34.0) Gecko/20100101 Firefox/73.0"]
                    & param "wd" .~ [query <> " site:baike.baidu.com"]
                    & param "ie" .~ ["utf-8"] & param "pn" .~ ["0"]
                    & param "cl" .~ ["3"] & param "rn" .~ ["100"]


processBaiduQuery :: (Text.Text, Update) -> IO [SendMsg]
processBaiduQuery (cmdBody, update) =
  if content /= ""
     then do
       result <- runBaiduSearch content
       logWT Info $
         "Query: [" <> Text.unpack content <> "] sending from " <> show (user_id update)
       pure [makeReqFromUpdate update result]
     else pure []
    where
      content = Text.strip cmdBody
