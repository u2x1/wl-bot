{-# LANGUAGE OverloadedStrings #-}
module Plugin.SolidotFetcher where

import           Core.Type.Unity.Request     as UR
import           Core.Data.Unity
import           Core.Type.Unity.Update      as UU
import           Core.Type.Universal
import           Control.Applicative
import           Network.Wreq                         (get, responseBody)
import           Control.Lens
import qualified Data.Text                   as Text
import           Data.Text.Read              as Text
import qualified Data.Text.IO                as Text
import qualified Data.Text.Lazy              as TextL
import           Data.Text.Lazy.Encoding
import qualified Data.ByteString.Lazy        as BL
import           Data.List
import           Data.Maybe
import           Data.Either
import           Utils.Misc

type Title       = BL.ByteString
type Link        = BL.ByteString
type Description = BL.ByteString

getSolidotContent :: IO [(Title, Link, Description)]
getSolidotContent = do
  r <- get "https://www.solidot.org/index.rss"
  pure $ splitEveryThree $ drop 1 $ searchAllBetweenBL "CDATA[" "]]" (r ^. responseBody)


splitEveryThree :: [a] -> [(a, a, a)]
splitEveryThree (x:y:z:rest) = (x,y,z):splitEveryThree rest
splitEveryThree _ = []

getElemContent :: BL.ByteString -> BL.ByteString
getElemContent = mconcat . searchAllBetweenBL ">" "<" . (">" <>) . (<> "<")

rmSubscribe :: (Text.Text, Update) -> IO [SendMsg]
rmSubscribe (_, update) = do
  fileContent <- Text.readFile (sfRqmt !! 1)
  if snd (Text.breakOn (Text.pack $ show (UU.chat_id update)) fileContent) == ""
     then do
      let subscribers = Text.splitOn "\n" fileContent
          afterRmUsers =
            filter (\user -> snd (Text.breakOn (Text.pack $ show (UU.chat_id update)) user) == "") subscribers
      _ <- Text.writeFile (sfRqmt !! 1) $ (mconcat.intersperse "\n") afterRmUsers
      pure [makeReqFromUpdate update "已取消对Solidot的订阅。"]
      else pure [makeReqFromUpdate update "您不在Solidot的订阅列表内。"]

addSubscriber :: (Text.Text, Update) -> IO [SendMsg]
addSubscriber (_, update) = do
  ss <- Text.readFile (sfRqmt !! 1)
  if snd (Text.breakOn (Text.pack $ show chatId) ss) == ""
     then do
       appendFile (sfRqmt !! 1) (show chatId <> " " <> show (platform update) <> " " <> show (message_type update) <> "\n")
       pure [makeReqFromUpdate update "订阅Solidot成功。"]
     else
       pure [makeReqFromUpdate update "您已在订阅列表内。"]
    where chatId = UU.chat_id update

parseSubscriber :: IO (Text.Text -> [SendMsg])
parseSubscriber = do
  fileContent <- Text.readFile (sfRqmt !! 1)
  let subscribers = Text.splitOn " " <$> Text.splitOn "\n" fileContent
  let infos = catMaybes $ getSubscriberInfos <$> subscribers
  pure $ sequence $ liftA2 uncurry3 (pure SendMsg) infos
  where
    uncurry3 f (a, b, c) = f a b c
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


checkNewOfSolidot :: IO [SendMsg]
checkNewOfSolidot = do
  originContent <- Text.readFile (head sfRqmt)
  newContent <- getSolidotContent
  if snd (Text.breakOn (TextL.toStrict.decodeUtf8.get1st $ head newContent) originContent) == ""
     then do
       BL.writeFile (head sfRqmt) $ mconcat.intersperse "\n" $ get1st <$> newContent
       f <- parseSubscriber
       pure $ reverse $ mconcat $ f.combineContent <$> takeWhile
         (\(title,_,_) -> snd (Text.breakOn ((TextL.toStrict . decodeUtf8) title) originContent) == "") newContent
     else pure []
    where
      get1st (x,_,_) = x
      combineContent :: (Title,Link,Description) -> Text.Text
      combineContent (title, link, dscrb) =
        TextL.toStrict $ decodeUtf8 $ title <> "\n\n" <> getElemContent dscrb <> "\n\n"<> link

sfRqmt :: [String]
sfRqmt = map ("wldata/" <>) ["SF-content.txt", "SF-subscriber.txt"]
