{-# LANGUAGE OverloadedStrings #-}
module Module.TorrentSearcher where

import Network.Wreq
--import qualified Data.Text as Text
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Utils.Misc as Misc
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Text as Text
import Data.ByteString.Lazy.Search as BL
import           Data.Text.Read              as Text
import Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Concurrent
import Data.Either
import Core.Type.Unity.Update
import Core.Type.Unity.Request
import Core.Data.Unity
import Data.Maybe

import Module.Pic

defaultQueryCnt :: Int
defaultQueryCnt = 10

getTrtLinks :: Int -> String -> IO (Maybe [String])
getTrtLinks num keyword = do
  r <- get $ "https://www.torrentkitty.tv/search/" <> keyword
  let links = Misc.searchAllBetweenBL "/information/" "\"" $ BL.drop 18000 $ r ^. responseBody
      linksTen = if length links > num then take num links else links
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

getTrtMag :: String -> IO (Maybe Text.Text)
getTrtMag url = do
  r <- get url
  pure $ ("magnet:?xt="<>).toStrict.decodeUtf8 <$> searchBetweenBL "magnet:?xt=" "<" (r ^. responseBody)

getTrtMags :: Int -> String -> IO ([Maybe Text.Text])
getTrtMags num keyword = do
  trtLinks <- getTrtLinks num keyword
  maybe' trtLinks (pure []) (\links -> do
    ch <- newChan
    _ <- liftIO $ traverse (\link -> forkIO (getTrtMag link >>= writeChan ch)) links
    replicateM (length links) (readChan ch))

generateTrtTexts :: String -> IO [[(String, FontDescrb)]]
generateTrtTexts keyword = do
  trtLinks <- getTrtLinks defaultQueryCnt keyword
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
        modifyHead ([]:_) _ = []

defaultFont :: FontDescrb
defaultFont = FontDescrb FontBlack FZHeiTi

redFont :: FontDescrb
redFont = FontDescrb FontRed FZHeiTi

concatTrtInfo :: TrtInfo -> [[(String, FontDescrb)]]
concatTrtInfo info = map (map (\(x, y) -> (toString x, y))) $
  [[((trt_name info <> "\t\t"), defaultFont)
  , (trt_date info, redFont)]] <>
  (map (uncurry (\a b -> [("\t\t|- " <> a <> "\t\t", defaultFont), (b, redFont)])) $
    filter (\(x,_) -> (snd $ BL.breakAfter "BitComet" x) == "") $ trt_files info)

processTrtQurey :: (Text.Text, Update) -> IO [SendMsg]
processTrtQurey (cmdBody, update) = do
  let mode = Text.splitOn " " cmdBody
  case length mode of
    0 -> pure []
    1 -> do
      texts <- generateTrtTexts $ Text.unpack cmdBody
      drawTextArray ("images/"<>imgCachePath) texts
      pure [makeReqFromUpdate'' update (Just imgCachePath) Nothing]
    2 -> do
      let i = fromRight 0 $ fst <$> decimal (mode !! 1)
      if i > defaultQueryCnt
         then pure [makeReqFromUpdate update $ "[错误] 索引超出界限。"]
         else do
           mags <- getTrtMags defaultQueryCnt (Text.unpack $ head mode)
           let infos = fromMaybe "Null" <$> mags
           pure [makeReqFromUpdate update $ "[磁链] " <> infos !! i]
    _ -> pure [makeReqFromUpdate update $ "[错误] 参数错误。"]
  where imgCachePath = "TrtCache.jpg"

type FileName = BL.ByteString
type FileSize = BL.ByteString
data TrtInfo = TrtInfo {
    trt_name :: BL.ByteString
  , trt_date :: BL.ByteString
  , trt_files :: [(FileName, FileSize)]
} deriving (Show)
