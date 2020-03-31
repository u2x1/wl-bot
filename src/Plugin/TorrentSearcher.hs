{-# LANGUAGE OverloadedStrings #-}
module Plugin.TorrentSearcher where

import Network.Wreq
import qualified Data.Text as Text
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Utils.Misc as Misc
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Lazy.Search as BL
import Control.Concurrent

get10TrtLinks :: String -> IO (Maybe [String])
get10TrtLinks keyword = do
  r <- get $ "https://www.torrentkitty.tv/search/" <> keyword
  let links = Misc.searchAllBetweenBL "/information/" "\"" $ BL.drop 18000 $ r ^. responseBody
      linksTen = if length links > 10 then take 10 links else links
  pure $ if null linksTen then Nothing else
    Just $ toString.("https://www.torrentkitty.tv/information/"<>) <$> linksTen

getTrtInfo :: String -> IO (Maybe TrtInfo)
getTrtInfo url = do
  r <- get url
  let rawHtml = r ^. responseBody
      tName = searchBetweenBL "title>" " - Torrent Kitty" rawHtml
      tDate = searchBetweenBL "Created On:</th><td>" "<" rawHtml
      tFiles = zip (searchAllBetweenBL "\"name\">" "<" rawHtml)
                     (searchAllBetweenBL "d class=\"size\">" "<" rawHtml)
  pure $ TrtInfo <$> tName <*> tDate <*> pure tFiles

generateTrtTexts :: String -> IO [String]
generateTrtTexts keyword = do
  trtLinks <- get10TrtLinks keyword
  maybe' trtLinks (pure []) (\links -> do
    ch <- newChan
    _ <- liftIO $ traverse (\link -> forkIO $ do
      info <- getTrtInfo link
      writeChan ch $ maybe' info [] concatTrtInfo) links
    (mconcat.addNumber) <$> replicateM (length links) (readChan ch))
  where addNumber xss = go xss 0
        go :: [[String]] -> Int -> [[String]]
        go (x:xs) cnt = (modifyHead x (("["<>show cnt<>"] ") <>)) : go xs (cnt + 1)
        go [] _ = []
        modifyHead (x:xs) f = (f x) : xs
        modifyHead [] _ = []


concatTrtInfo :: TrtInfo -> [String]
concatTrtInfo info = map toString $
  [" " <> trt_name info <> "\t\t-- " <> trt_date info] <>
  (map (uncurry (\a b -> "\t|- " <> a <> "\t\t" <> b)) $
    filter (\(x,_) -> (snd $ BL.breakAfter "BitComet" x) == "") $ trt_files info)

type FileName = BL.ByteString
type FileSize = BL.ByteString
data TrtInfo = TrtInfo {
    trt_name :: BL.ByteString
  , trt_date :: BL.ByteString
  , trt_files :: [(FileName, FileSize)]
} deriving (Show)
