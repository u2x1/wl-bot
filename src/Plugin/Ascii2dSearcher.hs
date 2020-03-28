{-# LANGUAGE OverloadedStrings #-}
module Plugin.Ascii2dSearcher where

import           Network.Wreq
import           Utils.Logging
import qualified Utils.Misc              as Misc
import           Core.Type.Unity.Update
import           Core.Type.Unity.Request
import           Core.Data.Unity
import           Control.Lens
import qualified Data.Text               as Text
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy          as TextL

type Url = Text.Text
getAscii2dUrl :: Url -> IO (Maybe (Url, Url))
getAscii2dUrl imgUrl = do
  r <- get $ "https://ascii2d.net/search/url/" <> Text.unpack imgUrl
  let rContent = r ^. responseBody
  let colorUrl = fixUrl.("/search/color"<>) <$> Misc.searchBetweenBL "/search/color" "\">" rContent
  let bovwUrl  = fixUrl.("/search/bovw"<>) <$> Misc.searchBetweenBL "/search/bovw" "\">" rContent
  pure $ (,) <$> colorUrl <*>bovwUrl
    where fixUrl = TextL.toStrict.decodeUtf8.("https://ascii2d.net"<>)


processAscii2dSearch ::  (Text.Text, Update) -> IO [SendMsg]
processAscii2dSearch (_, update) =
   case message_image_urls update of
     Just imgUrls -> do
       result <- getAscii2dUrl $ head imgUrls
       logWT Info $
         "Ascii2d Query: [" <> Text.unpack (head imgUrls) <> "] sending from " <> show (user_id update)
       case result of
         Just rst ->
               pure [makeReqFromUpdate update $ Misc.unlines
                  [ "[颜色搜索] " <> fst rst
                  , "[特征搜索] " <> snd rst]]
         _ -> pure [makeReqFromUpdate update "无结果。"]
     _ -> pure []

a2dHelps :: [Text.Text]
a2dHelps = ["{asc<PIC>} 从ascii2d.net搜索一张图片。"]
