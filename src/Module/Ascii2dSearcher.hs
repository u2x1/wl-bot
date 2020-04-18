{-# LANGUAGE OverloadedStrings #-}
module Module.Ascii2dSearcher where

import           Network.Wreq
import qualified Utils.Misc              as Misc
import           Core.Type.Unity.Update
import           Core.Type.Unity.Request
import           Core.Data.Unity
import           Control.Lens
import           Data.Text               as Text (Text)
import           Data.Text.Lazy.Encoding
import           Data.Text.Lazy          (toStrict)
import           Core.Type.EitherT

getAscii2dUrl :: String -> IO (Maybe (Text, Text))
getAscii2dUrl imgUrl' = do
  r <- get $ "https://ascii2d.net/search/url/" <> imgUrl'
  let rContent = r ^. responseBody
  let colorUrl = fixUrl.("color"<>) <$> Misc.searchBetweenBL "color" "\"" rContent
  let bovwUrl  = fixUrl.("bovw"<>) <$> Misc.searchBetweenBL  "bovw" "\"" rContent
  pure $ (,) <$> colorUrl <*> bovwUrl
    where fixUrl = toStrict.decodeUtf8.("https://ascii2d.net/search/"<>)

processAscii2dSearch ::  (Text, Update) -> IO [SendMsg]
processAscii2dSearch (_, update) = do
  x <- runMEitherT $ do
    imgUrl' <- liftMaybe "无效图片。" (pure $ head <$> message_image_urls update)
    result <- liftMaybe "无结果。"   (getAscii2dUrl imgUrl')
    pure $ Misc.unlines
       [ "[颜色搜索] " <> fst result
       , "[特征搜索] " <> snd result]
  pure [makeReqFromUpdate update $ getTextT x]
