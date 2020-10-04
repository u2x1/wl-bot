{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Module.HostlocFeed where

import           Control.Lens               ((^.))
import           Data.Attoparsec.ByteString
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.Either                (rights)
import           Data.Maybe
import           Network.Wreq               as Wreq (get, responseBody)

import           Data.ByteString.UTF8       (toString)
import           Data.Char                  (isDigit, ord)
import qualified Data.Text                  as T
import           Utils.Misc                 as Misc (searchAllBetweenBL,
                                                     searchBetweenBL, unlines)

import           Core.Type.Unity.Request    as UR
import           Core.Type.Universal
import           Text.RawString.QQ

import           Database.SQLite.Simple
-- import           Utils.ModuleHelper

checkNewOfHostloc :: IO [SendMsg]
checkNewOfHostloc = do
  texts <- withConnection "wldata.db" (\conn -> do
    hotThs <- catMaybes <$> (traverse (checkExist conn) =<< getHotThread)
    _ <- writeHotThs conn hotThs
    traverse trans2SendMsg hotThs)
  pure $ toTGChannelSM <$> texts

toTGChannelSM :: T.Text -> SendMsg
toTGChannelSM x = SendMsg "@hostloc_feed" "0" Private Telegram Nothing Nothing Nothing (Just x)

trans2SendMsg :: Thread -> IO T.Text
trans2SendMsg Thread{tid = t', name = n, time = t, reply = r', view = v} = do
  let link = "https://www.hostloc.com/thread-" <> show t' <> "-1-1.html"
      archv_link = "https://www.hostloc.com/archiver/tid-" <> show t' <> ".html"
  preview <- toString . BL.toStrict <$> getPreview archv_link
  return $ T.pack $ "<b>" <> n <> "</b>\n" <> hotness <> "\n\n" <> preview <> link
  where hotness = "(" <> (show r' <> " replies ") <> (show v <> " views ")  <> "in " <> show t <> " mins)"

getHotThread :: IO [Thread]
getHotThread = do
  rsp <- (^. responseBody) <$> Wreq.get site
  let b = searchAllBetweenBL "<th class=\"new\">\r\n" "</tr>" rsp
  let s = rights $ parseOnly thread . BL.toStrict <$> b
  return (filter isHot s)
  where
    site = "https://www.hostloc.com/forum.php?mod=forumdisplay&fid=45&filter=author&orderby=dateline"
    isHot :: Thread -> Bool
    isHot Thread{time = t, view = v, reply = r'} =
         t < 10 && (v > 60 || r' > 7)
      || t < 21 && (v > 100 || r' > 10)
      || t < 31 && (v > 200 || r' > 20)

getPreview :: String -> IO BL.ByteString
getPreview link = do
  rsp <- BL.drop 500 . (^. responseBody) <$> Wreq.get link
  let rst = fromMaybe "" $ searchBetweenBL "</h3" "\t\t\t" rsp
      realText = filter (not . BL.null) (BL.filter (\x -> x /= 13 && x /= 10 && x /= 9) <$> searchAllBetweenBL ">" "<" rst)
      retText = if BL.length (mconcat realText) > 300
                  then let x = Misc.unlines (Prelude.take 4 realText) in
                       if BL.length x > 500 then BL.take 500 x else x
                  else Misc.unlines realText
  return $ if BL.null retText then "" else retText <> "\n\n"

thread :: Parser Thread
thread = do
  tid'   <- str2Int . toString <$> (manyTill anyWord8 (string "tid=") *> takeTill (== 38))
  name'  <- toString . BS.pack <$> (manyTill anyWord8 (string "class=\"s xst\">") *> manyTill anyWord8 (string "</a>"))
  time'  <- timestr2timeint . toString <$> (manyTill anyWord8 (string "xi1\"><") *> takeTill (== 62) *> word8 62 *> takeTill (== 60))
  reply' <- str2Int . toString . BS.pack <$> (manyTill anyWord8 (string "xi2\">") *> manyTill anyWord8 (word8 60))
  view'  <- str2Int . toString . BS.pack <$> (string "/a><em>" *> manyTill anyWord8 (word8 60))
  return $ Thread tid' name' time' view' reply'

timestr2timeint :: String -> Int
timestr2timeint str = if null timeStr
                         then 30
                         else str2Int timeStr * timeScale
  where
        timeStr   = Prelude.takeWhile isDigit str
        timeScale = case str !! max 0 (length timeStr + 6) of
                      '秒' -> 0
                      '分' -> 1
                      '小' -> 60
                      _   -> 0

str2Int :: String -> Int
str2Int str = foldl (\a b -> a * 10 + b) 0 $ map toDigit str
  where toDigit w8 = ord w8 - 48

data Thread = Thread {
    tid   :: Int
  , name  :: String
  , time  :: Int
  , view  :: Int
  , reply :: Int
} deriving (Eq, Show)
instance ToRow Thread where
  toRow Thread{tid = t, name = n} = toRow (t, n)
instance FromRow Thread where
  fromRow = Thread <$> field <*> field <*> pure 0 <*> pure 0 <*> pure 0

initHostlocDB :: IO ()
initHostlocDB =
  withConnection "wldata.db" (\conn -> execute_ conn createHostlocDB)

createHostlocDB :: Query
createHostlocDB = [r|
CREATE TABLE IF NOT EXISTS hostloc
  ( tid INTEGER UNIQUE,
    name TEXT)
|]

checkExist :: Connection -> Thread -> IO (Maybe Thread)
checkExist conn th@Thread{tid = tid'} = do
  r' <- queryNamed conn qr [":tid" := tid'] :: IO [Thread]
  if null r' then pure (Just th) else pure Nothing
  where
    qr = "SELECT * FROM hostloc WHERE tid = :tid"

writeHotThs :: (Traversable t, ToRow q) => Connection -> t q -> IO (t ())
writeHotThs conn = traverse (execute conn "INSERT OR IGNORE INTO hostloc VALUES (?, ?)")

hlRqmt :: [String]
hlRqmt = ["wldata/HL-subscriber.txt"]
