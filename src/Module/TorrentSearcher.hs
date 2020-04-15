{-# LANGUAGE OverloadedStrings #-}
module Module.TorrentSearcher where

import           Network.Wreq
import           Control.Lens
import           Control.Monad
import           Utils.Misc                  as Misc
import qualified Data.ByteString.Lazy        as BL
import           Data.ByteString.Lazy.UTF8            (toString)
import qualified Data.Text                   as Text
import           Data.ByteString.Lazy.Search as BL
import           Data.Text.Read              as Text
import           Data.Text.Lazy                       (toStrict)
import           Data.Text.Lazy.Encoding              (decodeUtf8)
import           Control.Concurrent
import           Data.Either
import           Core.Type.Unity.Update
import           Core.Type.Unity.Request
import           Core.Data.Unity
import           Data.Maybe
import           Data.Aeson
import           Data.Vector                          ((!))
import           System.Directory

import           Module.Pic

defaultQueryCnt :: Int
defaultQueryCnt = 10

getTrtLinks :: Int -> String -> IO [String]
getTrtLinks num keyword = do
  r <- get $ "https://www.torrentkitty.tv/search/" <> keyword
  let links = Misc.searchAllBetweenBL "/information/" "\"" $ BL.drop 18000 $ r ^. responseBody
      linksTen = if length links > num then take num links else links
  pure $ toString.("https://www.torrentkitty.tv/information/"<>) <$> linksTen

getTrtInfos :: [String] -> IO [TrtInfo]
getTrtInfos links = do
  ch <- newChan
  _ <- traverse (\url -> forkIO $ do
    r <- get url
    let rawHtml = r ^. responseBody
        tName   = searchBetweenBL "title>" " - Torrent Kitty" rawHtml
        tDate   = searchBetweenBL "Created On:</th><td>" "<" rawHtml
        tFiles  = zip (searchAllBetweenBL "\"name\">" "<" rawHtml)
                      (searchAllBetweenBL "d class=\"size\">" "<" rawHtml)
        tMag    = (toStrict.decodeUtf8.("magnet:?xt="<>)) <$>
                     searchBetweenBL "magnet:?xt=" "<" (r ^. responseBody)
    writeChan ch $ TrtInfo <$> tName <*> tDate <*> pure tFiles <*> tMag) links
  catMaybes <$> replicateM (length links) (readChan ch)

generateTrtTexts :: [TrtInfo] -> IO [[(String, FontDescrb)]]
generateTrtTexts trtInfos = do
  pure $ mconcat.addNumber $ fmap concatTrtInfo trtInfos
  where addNumber = go 0
        go :: Int -> [[[(String,FontDescrb)]]] -> [[[(String, FontDescrb)]]]
        go cnt (x:xs) = (modifyHead x (("["<>show cnt<>"] ") <>)) : go (cnt + 1) xs
        go _ [] = []
        modifyHead (((x,y):ys):xs) f = ((f x, y):ys) : xs
        modifyHead [] _ = []
        modifyHead ([]:_) _ = []

tab :: BL.ByteString
tab = "    "

concatTrtInfo :: TrtInfo -> [[(String, FontDescrb)]]
concatTrtInfo info = map (map (\(x, y) -> ((toString.hLongText) x, y))) $
  [[((trt_name info <> "\t\t"), defaultFont)
  , (trt_date info, redFont)]] <>
  (map (uncurry (\a b -> [(tab <> "|- " <> a <> " ", defaultFont), (b, redFont)])) $
    filter (\(y,x) -> (snd $ BL.breakAfter "BitComet" y) == "" && ((snd $ BL.breakAfter "G" x) /= "" || (snd $ BL.breakAfter "M" x) /= "")) $
      trt_files info)
        where hLongText x = let len = BL.length x in
                                     if len > 170
                                        then (BL.take 160 x) <> " ... " <> (BL.drop (len-5) x)
                                        else x

processTrtQurey :: (Text.Text, Update) -> IO [SendMsg]
processTrtQurey (cmdBody, update) = do
  let mode = Text.splitOn " " cmdBody
  case length mode of
    0 -> pure []
    1 -> do
      let imgCachePath = "images/TR-" <> (Text.unpack cmdBody) <> ".jpg"
      exist <- doesFileExist $ imgCachePath
      if not exist
         then do
           infos <- getTrtInfos =<< (getTrtLinks defaultQueryCnt $ Text.unpack cmdBody)
           texts <- generateTrtTexts infos
           drawTextArray imgCachePath 50 texts
           BL.writeFile ("wldata/TR-" <> Text.unpack cmdBody <> ".txt") $ encode $ fmap trt_mag infos
         else pure ()
      pure [makeReqFromUpdate'' update (drop 7 imgCachePath) ""]
    2 -> do
      let i = fromRight 0 $ fst <$> decimal (mode !! 1)
      if i > defaultQueryCnt
         then pure [makeReqFromUpdate update $ "[错误] 索引超出界限。"]
         else do
           let magCachePath = "wldata/TR-" <> (Text.unpack $ head mode) <>".txt"
           exist <- doesFileExist magCachePath
           if not exist
              then pure [makeReqFromUpdate update $ "[错误] 本地无缓存。"]
              else do
                mags <- eitherDecode <$> BL.readFile magCachePath :: IO (Either String Array)
                case mags of
                  Left err -> pure [makeReqFromUpdate update $ "[错误] " <> Text.pack err]
                  Right lst -> pure [makeReqFromUpdate update $ "[磁链] " <> (Text.pack.show $ lst ! i)]
    _ -> pure [makeReqFromUpdate update $ "[错误] 参数错误。"]

type FileName = BL.ByteString
type FileSize = BL.ByteString
data TrtInfo = TrtInfo {
    trt_name :: BL.ByteString
  , trt_date :: BL.ByteString
  , trt_files :: [(FileName, FileSize)]
  , trt_mag :: Text.Text
} deriving (Show)

defaultFont :: FontDescrb
defaultFont = FontDescrb FontBlack FZHeiTi
redFont :: FontDescrb
redFont = FontDescrb FontRed FZHeiTi
