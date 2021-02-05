{-# LANGUAGE OverloadedStrings #-}
module Module.YandeFetcher where

import           Control.Lens
import           Core.Type.Unity.Request       as UR
import           Core.Type.Universal
import qualified Data.ByteString.Lazy          as BL
import           Data.ByteString.Lazy.UTF8     as UTF8
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Network.Wreq
import           Utils.Misc

getPopImgUrls :: IO [String]
getPopImgUrls = do
  let opts = defaults & header "Cookie" .~ [""]
  r <- getWith opts "https://yande.re/post/popular_recent"
  pure
    $   toString
    .   ("https://files.yande.re/sample/" <>)
    .   (<> "/yande.re.jpg")
    <$> searchAllBetweenBL "/sample/" "/" (BL.drop 100000 $ r ^. responseBody)

checkYandePopImgs :: IO [SendMsg]
checkYandePopImgs = do
  imgUrls' <- getPopImgUrls
  subs     <- parseYandeSubscriber
  pure $ mconcat $ subs . T.pack <$> imgUrls'

parseYandeSubscriber :: IO (T.Text -> [SendMsg])
parseYandeSubscriber = do
  fileContent <- T.readFile (head ydRqmt)
  let subscribers = T.splitOn " " <$> T.splitOn "\n" fileContent
  let infos       = catMaybes $ getSubscriberInfos <$> subscribers
  pure $ traverse (uncurry3 SendMsg) infos
 where
  uncurry3 f (a, b, c) x = f a a b c Nothing (Just x) Nothing Nothing
  getSubscriberInfos [userId, plat, targetType] = Just
    ( T.unpack userId
    , case targetType of
      "Private" -> Private
      "Group"   -> Group
      _         -> error "Unrecognized target type."
    , case plat of
      "Telegram" -> Telegram
      "QQ"       -> QQ
      _          -> error "Unrecognized platform."
    )
  getSubscriberInfos _ = Nothing

ydRqmt :: [String]
ydRqmt = ["wldata/YD-subscribers.txt"]
