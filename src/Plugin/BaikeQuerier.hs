{-# LANGUAGE OverloadedStrings #-}
module Plugin.BaikeQuerier where

import           Core.Type.Unity.Update       as UU
import           Core.Type.Unity.Request      as UR
import           Core.Data.Unity

import           Utils.Logging

import           Network.Wreq
import           Control.Lens
import qualified Data.ByteString              as BS    (ByteString)
import qualified Data.ByteString.Lazy.UTF8    as UTF8  (toString)
import           Data.ByteString.Lazy         as BL
import           Data.ByteString.Lazy.Search           (breakOn, breakAfter, replace)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as TextL
import           Data.Text.Lazy.Encoding

searchBetween :: BS.ByteString -> BS.ByteString -> BL.ByteString -> BL.ByteString
searchBetween left right content =
  let fstround = snd $ breakAfter left content in
      fst (breakOn right fstround)

getFstUrl :: BL.ByteString -> Maybe String
getFstUrl content = fixUrl $ UTF8.toString $ searchBetween "baike.baidu.com/item" "\"" (BL.drop 180000 content)
  where fixUrl "" = Nothing
        fixUrl xs = Just $ "https://baike.baidu.com/item" ++ xs

getWords :: ByteString -> [ByteString]
getWords "" = []
getWords str = fst (breakOn "<" xs) : getWords xs
  where xs = snd $ breakAfter ">" str

-- Select fragments that are not equal to "&nbsp;" or started with "\n"
concatWord :: [ByteString] -> ByteString
concatWord oStr = replace "&quot;" ("\""::BL.ByteString) (replace "\n" (""::BL.ByteString) s)
  where s = BL.concat $ Prelude.filter
              (\str -> not (BL.null str) && str /= "&nbsp;" && BL.take 2 str /= "\n[")
              oStr

runBaiduSearch :: String -> IO Text.Text
runBaiduSearch query = do
  result <- getWith opts $ "https://www.baidu.com/s?wd=" ++ query ++ " site:baike.baidu.com&ie=utf-8&pn=0&cl=3&rn=100"
  case getFstUrl (result ^. responseBody) of
    Nothing      -> pure "无结果。"
    Just realUrl -> do
      realRsp <- get realUrl
      case concatWord $ getWords $ getFirstPara $ realRsp ^. responseBody of
        "" -> pure (Text.pack $
                     "已为您找到一个词条，但此词条无摘要。查看此处:\n" <> realUrl)
        resultText  -> pure $ TextL.toStrict $ decodeUtf8 resultText
  where
    getFirstPara = searchBetween "<div class=\"lemma-summary\" label-module=\"lemmaSummary\"" "lemmaWgt"
    opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.1; WOW64; rv:34.0) Gecko/20100101 Firefox/73.0"]

processQuery :: (Text.Text, Update) -> IO [SendMsg]
processQuery (cmdBody, update) =
  if content /= ""
     then do
       result <- runBaiduSearch $ Text.unpack content
       logWT Info $
         "Query: [" <> Text.unpack content <> "] sending from " <> show (user_id update)
       pure [makeReqFromUpdate update result]
     else pure [makeReqFromUpdate update baikeHelps]
    where
      content = Text.strip cmdBody

baikeHelps :: Text.Text
baikeHelps = Text.unlines [ "====BaikeQuerier===="
                          , "/bk ENTRYNAME: 从baike.baidu.com查询词条"]
