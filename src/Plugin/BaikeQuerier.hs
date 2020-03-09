{-# LANGUAGE OverloadedStrings #-}
module Plugin.BaikeQuerier where

import           Core.Web.CoolQ               as CQ
import           Core.Web.Telegram            as TG
import           Core.Type.CoolQ.Update       as CQ
import           Core.Type.Telegram.Update    as TG
import           Core.Data.CoolQ              as CQ
import           Core.Data.Telegram           as TG

import           Utils.Config

import           Network.Wreq                 as Wreq
import           Control.Lens
import           Control.Concurrent                    (ThreadId)
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
  result <- Wreq.getWith opts $ "https://www.baidu.com/s?&wd=" ++ query ++ " site%3Abaike.baidu.com%20&oq=" ++ query ++ " site%253Abaike.baidu.com&ie=utf-8&rqlang=cn&rsv_enter=1"
--  BL.writeFile "testing.html" (result ^. responseBody)
  case getFstUrl (result ^. responseBody) of
    Nothing  -> pure "No result found."
    Just realUrl -> do
      realRsp <- Wreq.get realUrl
      pure $ filterConcatWord $ getWords $ getFirstPara $ realRsp ^. responseBody
  where
    getFirstPara = searchBetween "<div class=\"lemma-summary\" label-module=\"lemmaSummary\"" "/div>"
    opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (X11; Linux x86_64; rv:73.0) Gecko/20100101 Firefox/73.0"]

processCQQuery :: Config -> CQ.Update -> IO (Maybe ThreadId)
processCQQuery config cqUpdate
  | not $ searchOn config = pure Nothing
  | otherwise =
    let msgTxt = getText (CQ.message cqUpdate) in
     if Text.take 4 msgTxt == "/qr " && Text.replace " " "" msgTxt /= "/qr"
       then do
         result <- runBaiduSearch $ Text.unpack $ Text.strip (Text.drop 4 msgTxt)
         let resultText = Just (TextL.toStrict $ decodeUtf8 result)
         Just <$> CQ.sendBackTextMsg (fromMaybe "" resultText) cqUpdate config
       else pure Nothing

processTGQuery :: Config -> TG.Update -> IO (Maybe ThreadId)
processTGQuery config tgUpdate
  | not $ searchOn config = pure Nothing
  | otherwise =
    let msgTxt =
          case snd $ TG.getMessageFromUpdate tgUpdate of
            Just msg -> fromMaybe "" (TG.text msg)
            _        -> ""
    in
     if Text.take 4 msgTxt == "/qr " && Text.replace " " "" msgTxt /= "/qr"
       then do
         result <- runBaiduSearch $ Text.unpack $ Text.strip (Text.drop 4 msgTxt)
         let resultText = Just (TextL.toStrict $ decodeUtf8 result)
         Just <$> TG.sendBackTextMsg (fromMaybe "" resultText) tgUpdate config
       else pure Nothing
