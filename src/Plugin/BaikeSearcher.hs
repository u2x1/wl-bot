{-# LANGUAGE OverloadedStrings #-}
module Plugin.BaikeSearcher where

import Core.Web.CoolQ                       as CQ
import Core.Web.Telegram                    as TG

import Core.Type.CoolQ.Update               as CQ
import Core.Type.CoolQ.SendMsg              as CQ
import Core.Type.Telegram.Update            as TG

import Core.Data.CoolQ                      as CQ
import Core.Data.Telegram                   as TG

import Utils.Config
import Utils.Logging

import           Network.Wreq               as Wreq
import           Control.Lens
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.ByteString.Lazy       as BL
import           Data.ByteString.Lazy.Search
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as TextL
import Data.Text.Lazy.Encoding
import Data.Aeson
import Control.Concurrent
import Control.Concurrent.Lock

searchBetween :: BS.ByteString -> BS.ByteString -> ByteString -> ByteString
searchBetween left right content =
  let fstround = snd $ breakAfter left content in
      fst (breakOn right fstround)

getFstUrl :: ByteString -> String
getFstUrl content = fixUrl $ Char8.unpack $ searchBetween "<a class=\"result-title\" href=\"" "\"" content
  where fixUrl "" = ""
        fixUrl xs = if Prelude.head xs == '/'
                       then "https://baike.baidu.com" <> xs
                       else  xs

getWords :: ByteString -> [ByteString]
getWords "" = []
getWords str = fst (breakOn "<" xs) : getWords xs
  where xs = snd $ breakAfter ">" str

-- Select fragments that are not equal to "&nbsp;" or started with "\n"
filterConcatWord :: [ByteString] -> ByteString
filterConcatWord oStr = replace "\n" (""::BL.ByteString) s
  where s = BL.concat $ Prelude.filter
              (\str -> not (Char8.null str) && str /= "&nbsp;" && Char8.take 2 str /= "\n[")
              oStr

runBaikeSearch :: String -> IO ByteString
runBaikeSearch query = do
  result <- Wreq.get $ "https://baike.baidu.com/search?word=" ++ query
  realRsp <- Wreq.get $ getFstUrl (result ^. responseBody)
  pure $ filterConcatWord $ getWords $ getFirstPara $ realRsp ^. responseBody
  where
    getFirstPara = searchBetween "<div class=\"para\" label-module=\"para\"" "</div>"

processCQQuery :: Lock -> Config -> CQ.Update -> IO (Maybe ThreadId)
processCQQuery lock config cqUpdate
  | not $ searchOn config = pure Nothing
  | otherwise =
    let cqSvr = cqServer config in
    if Text.take 4 msgTxt == "/qr "
       then do
         result <- runBaikeSearch $ Text.unpack $ Text.strip (Text.drop 4 msgTxt)
         let req = toJSON $ SendMsg <$> group_id cqUpdate <*> Just (TextL.toStrict $ decodeUtf8 result)
         Just <$> postCqRequest lock cqSvr "send_group_msg" req
       else pure Nothing
    where
      msgTxt = getText (CQ.message cqUpdate)
