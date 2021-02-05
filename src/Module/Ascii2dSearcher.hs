{-# LANGUAGE OverloadedStrings #-}
module Module.Ascii2dSearcher where

import           Control.Lens
import           Core.Data.Unity
import           Core.Type.EitherT
import           Core.Type.Unity.Request
import           Core.Type.Unity.Update        as UU
import           Data.Text                     as Text
                                                ( Text
                                                , pack
                                                )
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text.Lazy.Encoding
import           Network.Wreq
import qualified Utils.Misc                    as Misc

getAscii2dUrl :: String -> IO (Maybe (Text, Text))
getAscii2dUrl imgUrl' = do
  r <- get $ "https://ascii2d.net/search/url/" <> imgUrl'
  let rContent = r ^. responseBody
  let colorUrl =
        fixUrl . ("color" <>) <$> Misc.searchBetweenBL "color" "\"" rContent
  let bovwUrl =
        fixUrl . ("bovw" <>) <$> Misc.searchBetweenBL "bovw" "\"" rContent
  pure $ (,) <$> colorUrl <*> bovwUrl
  where fixUrl = toStrict . decodeUtf8 . ("https://ascii2d.net/search/" <>)

processAscii2dSearch :: (Text, Update) -> IO [SendMsg]
processAscii2dSearch (_, update) = do
  x <- runMEitherT $ do
    imgUrl' <- liftList "无效图片。" (pure $ message_image_urls update)
    result  <- liftMaybe "无结果。" (getAscii2dUrl (head imgUrl'))
    pure $ Misc.unlines
      [ "[Ascii2dC] " <> fst result
      , "[Ascii2dB] " <> snd result
      , "[Yandex] " <> yandexHost <> pack (head imgUrl')
      ]
  pure [makeReqFromUpdate update $ getTextT x]
 where
  yandexHost =
    "https://yandex.com/images/search?source=collections&rpt=imageview&url="
