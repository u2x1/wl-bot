{-# LANGUAGE OverloadedStrings #-}
module Plugin.BaikeQuerier where

import           Core.Web.CoolQ               as Q
import           Core.Web.Telegram            as T
import           Core.Type.Unity.Update       as UU
import           Core.Type.Unity.Request      as UR

import           Utils.Config
import           Utils.Logging

import           Network.Wreq                 as Wreq
import           Control.Lens
import qualified Data.ByteString              as BS    (ByteString)
import qualified Data.ByteString.Lazy.UTF8    as UTF8  (toString)
import           Data.ByteString.Lazy         as BL
import           Data.ByteString.Lazy.Search           (breakOn, breakAfter, replace)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as TextL (toStrict)
import           Data.Text.Lazy.Encoding
import           Data.Maybe

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
filterConcatWord :: [ByteString] -> ByteString
filterConcatWord oStr = replace "&quot;" ("\""::BL.ByteString) (replace "\n" (""::BL.ByteString) s)
  where s = BL.concat $ Prelude.filter
              (\str -> not (BL.null str) && str /= "&nbsp;" && BL.take 2 str /= "\n[")
              oStr

runBaiduSearch :: String -> IO ByteString
runBaiduSearch query = do
  result <- Wreq.getWith opts $ "https://www.baidu.com/s?wd=" ++ query ++ " site%3Abaike.baidu.com%20&ie=utf-8&pn=0&cl=3&rn=100"
--  BL.writeFile "testing.html" (result ^. responseBody)
  case getFstUrl (result ^. responseBody) of
    Nothing  -> pure "No result found."
    Just realUrl -> do
      realRsp <- Wreq.get realUrl
      --logWT Debug ("Url: " <> realUrl)
      pure $ filterConcatWord $ getWords $ getFirstPara $ realRsp ^. responseBody
  where
    getFirstPara = searchBetween "<div class=\"lemma-summary\" label-module=\"lemmaSummary\"" "/div>"
    opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (X11; Linux x86_64; rv:73.0) Gecko/20100101 Firefox/73.0"]

processQuery :: Update -> IO [SendMsg]
processQuery update =
  if Text.take 3 msgTxt == "/qr" && Text.replace " " "" msgTxt /= "/qr"
    then do
      result <- runBaiduSearch $ Text.unpack $ Text.strip (Text.drop 3 msgTxt)
      let resultText = Just (TextL.toStrict $ decodeUtf8 result)
      logWT Info $
        "Query: [" <> Text.unpack msgTxt <> "] sending from " <> show (user_id update)
      pure [SendMsg (fromMaybe "" resultText) (UU.chat_id update) (message_type update) (platform update)]
    else pure []
    where
      msgTxt = message_text update

processCQQuery :: Config -> Update -> IO (Maybe (Response BL.ByteString))
processCQQuery config update =
  if Text.take 4 msgTxt == "/qr " && Text.replace " " "" msgTxt /= "/qr"
    then do
      result <- runBaiduSearch $ Text.unpack $ Text.strip (Text.drop 4 msgTxt)
      let resultText = Just (TextL.toStrict $ decodeUtf8 result)
      logWT Info $
        "Query: [" <> Text.unpack msgTxt <> "] sending from " <> show (user_id update)
      Just <$> Q.sendBackTextMsg (fromMaybe "" resultText) update config
    else pure Nothing
    where
      msgTxt = message_text update

processTGQuery :: Config -> Update -> IO (Maybe (Response BL.ByteString))
processTGQuery config update =
  if Text.take 4 msgTxt == "/qr " && Text.replace " " "" msgTxt /= "/qr"
    then do
      result <- runBaiduSearch $ Text.unpack $ Text.strip (Text.drop 4 msgTxt)
      let resultText = Just (TextL.toStrict $ decodeUtf8 result)
      logWT Info $
        "Query: [" <> Text.unpack msgTxt <> "] sending from " <> show (user_id update)
      Just <$> T.sendBackTextMsg (fromMaybe "" resultText) update config
    else pure Nothing
    where
      msgTxt = message_text update
