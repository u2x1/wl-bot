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

import Plugin.Pic

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

generateTrtTexts :: String -> IO [[(String, FontDescrb)]]
generateTrtTexts keyword = do
  trtLinks <- get10TrtLinks keyword
  maybe' trtLinks (pure []) (\links -> do
    ch <- newChan
    _ <- liftIO $ traverse (\link -> forkIO $ do
      info <- getTrtInfo link
      writeChan ch $ maybe' info [] concatTrtInfo) links
    (mconcat.addNumber) <$> replicateM (length links) (readChan ch))
  where addNumber = go 0
        go :: Int -> [[[(String,FontDescrb)]]] -> [[[(String, FontDescrb)]]]
        go cnt (x:xs) = (modifyHead x (("["<>show cnt<>"] ") <>)) : go (cnt + 1) xs
        go _ [] = []
        modifyHead (((x,y):ys):xs) f = ((f x, y):ys) : xs
        modifyHead [] _ = []

defaultFont = FontDescrb FontBlack FZHeiTi
redFont = FontDescrb FontRed FZHeiTi

concatTrtInfo :: TrtInfo -> [[(String, FontDescrb)]]
concatTrtInfo info = map (map (\(x, y) -> (toString x, y))) $
  [[((trt_name info <> "\t\t"), defaultFont)
  , (trt_date info, redFont)]] <>
  (map (uncurry (\a b -> [("\t\t|- " <> a <> "\t\t", defaultFont), (b, redFont)])) $
    filter (\(x,_) -> (snd $ BL.breakAfter "BitComet" x) == "") $ trt_files info)

type FileName = BL.ByteString
type FileSize = BL.ByteString
data TrtInfo = TrtInfo {
    trt_name :: BL.ByteString
  , trt_date :: BL.ByteString
  , trt_files :: [(FileName, FileSize)]
} deriving (Show)
