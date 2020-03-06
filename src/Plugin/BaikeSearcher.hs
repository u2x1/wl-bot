{-# LANGUAGE OverloadedStrings #-}
module Plugin.BaikeSearcher where

import           Network.Wreq
import           Control.Lens
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.ByteString.Lazy       as BL
import           Data.ByteString.Lazy.Search

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
  result <- get $ "https://baike.baidu.com/search?word=" ++ query
  realRsp <- get $ getFstUrl $ result ^. responseBody
  pure $ filterConcatWord $ getWords $ getFirstPara $ realRsp ^. responseBody
  where
    getFirstPara = searchBetween "<div class=\"para\" label-module=\"para\"" "</div>"
