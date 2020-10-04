{-# LANGUAGE OverloadedStrings #-}
module Module.SolidotFetcher where

import           Control.Lens
import           Core.Data.Unity
import           Core.Type.Unity.Request as UR
import           Core.Type.Unity.Update  as UU
import           Core.Type.Universal
import qualified Data.ByteString.Lazy    as BL
import           Data.List
import           Data.Maybe
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Encoding
import           Network.Wreq            (get, responseBody)
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
splitEveryThree _            = []

getElemContent :: BL.ByteString -> BL.ByteString
getElemContent = mconcat . searchAllBetweenBL ">" "<" . (">" <>) . (<> "<")

rmSubscribe :: (T.Text, Update) -> IO [SendMsg]
rmSubscribe (_, update) = do
  fileContent <- T.readFile (sfRqmt !! 1)
  if snd (T.breakOn (T.pack $ show (UU.chat_id update)) fileContent) == ""
     then pure [makeReqFromUpdate update "您不在Solidot的订阅列表内。"]
     else do
      let subscribers = T.splitOn "\n" fileContent
          afterRmUsers =
            filter (\user -> snd (T.breakOn (T.pack $ show (UU.chat_id update)) user) == "") subscribers
      _ <- T.writeFile (sfRqmt !! 1) $ (mconcat.intersperse "\n") afterRmUsers
      pure [makeReqFromUpdate update "取消了对Solidot的订阅。"]

addSubscriber :: (T.Text, Update) -> IO [SendMsg]
addSubscriber (_, update) = do
  ss <- T.readFile (sfRqmt !! 1)
  if snd (T.breakOn (T.pack $ show chatId) ss) == ""
     then do
       appendFile (sfRqmt !! 1) (show chatId <> " " <> show (platform update) <> " " <> show (message_type update) <> "\n")
       pure [makeReqFromUpdate update "已订阅Solidot。"]
     else
       pure [makeReqFromUpdate update "您已在订阅列表内。"]
    where chatId = UU.chat_id update

parseSubscriber :: IO (T.Text -> [SendMsg])
parseSubscriber = do
  fileContent <- T.readFile (sfRqmt !! 1)
  let subscribers = T.splitOn " " <$> T.splitOn "\n" fileContent
  let infos = catMaybes $ getSubscriberInfos <$> subscribers
  pure $ traverse (uncurry3 SendMsg) infos . Just
  where
    uncurry3 f (a, b, c) = f a a b c Nothing Nothing Nothing
    getSubscriberInfos [userId, plat, targetType] = Just
      ( T.unpack userId
      , case targetType of
          "Private" -> Private
          "Group"   -> Group
          _         -> error "Unrecognized target type."
      , case plat of
          "Telegram" -> Telegram
          "QQ"       -> QQ
          _          -> error "Unrecognized platform.")
    getSubscriberInfos _ = Nothing

checkNewOfSolidot :: IO [SendMsg]
checkNewOfSolidot = do
  originContent <- T.readFile (head sfRqmt)
  newContent <- getSolidotContent
  if snd (T.breakOn (TL.toStrict.decodeUtf8.get1st $ head newContent) originContent) == ""
     then do
       BL.writeFile (head sfRqmt) $ mconcat.intersperse "\n" $ get1st <$> newContent
       f <- parseSubscriber
       pure $ reverse $ mconcat $ f.combineContent <$> takeWhile
         (\(title,_,_) -> snd (T.breakOn ((TL.toStrict . decodeUtf8) title) originContent) == "") newContent
     else pure []
    where
      get1st (x,_,_) = x
      combineContent :: (Title,Link,Description) -> T.Text
      combineContent (title, link, dscrb) =
        TL.toStrict $ decodeUtf8 $ title <> "\n\n" <> getElemContent dscrb <> "\n\n"<> link

sfRqmt :: [String]
sfRqmt = map ("wldata/" <>) ["SF-content.txt", "SF-subscriber.txt"]
