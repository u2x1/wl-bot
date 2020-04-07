{-# LANGUAGE OverloadedStrings #-}
module Module.YandeFetcher where

import           Core.Type.Unity.Request     as UR
import           Core.Type.Universal
import           Network.Wreq
import           Control.Lens
import qualified Data.Text                   as Text
import           Data.Text.Read              as Text
import qualified Data.Text.IO                as Text
import qualified Data.ByteString.Lazy        as BL
import           Data.ByteString.Lazy.UTF8        as UTF8
import           Data.Maybe
import           Data.Either
import           Utils.Misc

getPopImgUrls :: IO [String]
getPopImgUrls = do
  let opts = defaults & header "Cookie" .~ ["yande.re=L1hnZUgvVy9yaXZlaEt1NUVDR0lSZHhJamhnMHhmQmkzdVNvdTByTkFIQUhqZFlHQm8xWUREL0xxNnlTUnBpQlRDVTJ5RU9IdWdHejRlQUlpREJ5c1FqZ3RMM29kYndvVWFjV3QxWEhnVWR4eTVRZlpzaFRjZEZXMG1YeHhWZTdaRkNITmlJdCtGMVFicTQ1ZEV5dDVES3VOYnF4bmsrUHBPdm9LL3dBSFVVPS0tc3VySTJ5MDhYNnZsZXBPbVhodGdLdz09--747324cc50cbcdd59d1a82c434c19ba7ac1aea93;domain=.yande.re;path=/;"]
  r <- getWith opts "https://yande.re/post/popular_recent"
  pure $ (toString.("https://files.yande.re/sample/"<>).(<>"/yande.re.jpg")) <$>
    (searchAllBetweenBL "/sample/" "/" (BL.drop 100000 $ r ^. responseBody))

sendYandePopImgs :: IO [SendMsg]
sendYandePopImgs = do
  imgUrls' <- getPopImgUrls
  subs <- parseYandeSubscriber
  pure $ mconcat $ (subs . Text.pack) <$> imgUrls'

parseYandeSubscriber :: IO (Text.Text -> [SendMsg])
parseYandeSubscriber = do
  fileContent <- Text.readFile (ydRqmt !! 0)
  let subscribers = Text.splitOn " " <$> Text.splitOn "\n" fileContent
  let infos = catMaybes $ getSubscriberInfos <$> subscribers
  pure $ (traverse (uncurry3 SendMsg) infos)
  where
    uncurry3 f (a, b, c) = (\x -> f a b c Nothing (Just [x]) Nothing Nothing)
    getSubscriberInfos [userId, plat, targetType] = Just
      ( fromRight 0 $ fst <$> decimal userId
      , case targetType of
          "Private" -> Private
          "Group"   -> Group
          _          -> error "Unrecognized target type."
      , case plat of
          "Telegram" -> Telegram
          "QQ"       -> QQ
          _          -> error "Unrecognized platform.")
    getSubscriberInfos _ = Nothing

ydRqmt :: [String]
ydRqmt = ["wldata/YD-subscribers.txt"]
