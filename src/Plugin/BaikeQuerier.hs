{-# LANGUAGE OverloadedStrings #-}
module Plugin.BaikeQuerier where

import           Core.Type.Unity.Update       as UU
import           Core.Type.Unity.Request      as UR

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

searchBetween :: BS.ByteString -> BS.ByteString -> ByteString -> ByteString
searchBetween left right content =
  let fstround = snd $ breakAfter left content in
      fst (breakOn right fstround)

getFstUrl :: ByteString -> Maybe String
getFstUrl content = fixUrl $ UTF8.toString $ searchBetween "baike.baidu.com/item" "\"" (BL.drop 200000 content)
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
  result <- getWith opts $ "https://www.baidu.com/s?wd=" ++ query ++ " site%3Abaike.baidu.com%20&ie=utf-8&pn=0&cl=3&rn=100"
  case getFstUrl (result ^. responseBody) of
    Nothing      -> pure "No result found."
    Just realUrl -> do
      realRsp <- get realUrl
      case concatWord $ getWords $ getFirstPara $ realRsp ^. responseBody of
        "" -> pure (Text.pack $
                     "An entry is found for you, but it doesn't have a summary. Check this: " <> realUrl)
        resultText  -> pure $ TextL.toStrict $ decodeUtf8 resultText
  where
    getFirstPara = searchBetween "<div class=\"lemma-summary\" label-module=\"lemmaSummary\"" "/div>"
    opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (X11; Linux x86_64; rv:73.0) Gecko/20100101 Firefox/73.0"]

processQuery :: (Text.Text, Update) -> IO [SendMsg]
processQuery (cmdBody, update) =
  if content /= ""
     then do
       result <- runBaiduSearch $ Text.unpack content
       logWT Info $
         "Query: [" <> Text.unpack content <> "] sending from " <> show (user_id update)
       pure [SendMsg result (UU.chat_id update) (message_type update) (platform update)]
     else pure [SendMsg baikeHelps (UU.chat_id update) (message_type update) (platform update)]
    where
      content = Text.strip cmdBody

baikeHelps :: Text.Text
baikeHelps = "==BaikeQuerier==\n\
             \/bkqr ENTRYNAME: Query an entry from baike.baidu.com"
