{-# LANGUAGE OverloadedStrings #-}
module Module.TorrentSearcher where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Core.Data.Unity
import           Core.Type.Unity.Request
import           Core.Type.Unity.Update
import           Data.Aeson
import qualified Data.ByteString.Lazy        as BL
import           Data.ByteString.Lazy.Search as BL
import           Data.ByteString.Lazy.UTF8   (toString)
import           Data.Either
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                   as Text
import           Data.Text.Lazy              (toStrict)
import           Data.Text.Lazy.Encoding     (decodeUtf8)
import           Data.Text.Read              as Text
import           Data.Vector                 ((!))
import           Network.Wreq
import           System.Directory
import           Utils.Misc                  as Misc

import           Module.Pic

defaultQueryCnt :: Int
defaultQueryCnt = 10

getTrtLinks :: Int -> String -> IO (Maybe [String])
getTrtLinks num keyword = do
  r <- get $ "https://www.torrentkitty.tv/search/" <> keyword
  let links = Misc.searchAllBetweenBL "/information/" "\"" $ BL.drop 18000 $ r ^. responseBody
      linksTen = if length links > num then take num links else links
  if null linksTen
     then pure Nothing
     else pure . Just $ toString.("https://www.torrentkitty.tv/information/"<>) <$> linksTen

getTrtInfos :: [String] -> IO [TrtInfo]
getTrtInfos links = do
  ch <- newChan
  traverse_ (\url -> forkIO $ do
    r <- get url
    let rawHtml = r ^. responseBody
        tName   = searchBetweenBL "title>" " - Torrent Kitty" rawHtml
        tDate   = searchBetweenBL "Created On:</th><td>" "<" rawHtml
        tFiles  = zip (searchAllBetweenBL "\"name\">" "<" rawHtml)
                      (searchAllBetweenBL "d class=\"size\">" "<" rawHtml)
        tMag    = toStrict.decodeUtf8.("magnet:?xt="<>) <$>
                     searchBetweenBL "magnet:?xt=" "<" (r ^. responseBody)
    writeChan ch $ TrtInfo <$> tName <*> tDate <*> pure tFiles <*> tMag) links
  catMaybes <$> replicateM (length links) (readChan ch)

generateTrtTexts :: [TrtInfo] -> IO [[DrawText]]
generateTrtTexts trtInfos =
  pure $ addInfo.mconcat.addNumber $ fmap concatTrtInfo trtInfos
  where
    addInfo = ([DrawText "==数据来自TorrentKitty==" redFont] :)
    addNumber = go 0
    go :: Int -> [[[DrawText]]] -> [[[DrawText]]]
    go cnt (x:xs) = modifyHead x (("["<>show cnt<>"] ") <>) : go (cnt + 1) xs
    go _ []       = []
    modifyHead (((DrawText x y):ys):xs) f = (DrawText (f x) y : ys) : xs
    modifyHead [] _                       = []
    modifyHead ([]:_) _                   = []

tab :: String
tab = "    "

concatTrtInfo :: TrtInfo -> [[DrawText]]
concatTrtInfo info =
  [[DrawText (toString (trt_name info) <> "\t\t") defaultFont,
    DrawText (toString (trt_date info)) redFont]] <>
  addIndent (filter notUselessInfo $ trt_files info)
  where
    notUselessInfo (y, x) = snd (BL.breakAfter "BitComet" y) == ""
                        && (snd (BL.breakAfter "G" x) /= "" || snd (BL.breakAfter "M" x) /= "")
    hLongText x = let len = BL.length x in
                        if len > 170
                           then BL.take 160 x <> " ... " <> BL.drop (len-5) x
                           else x
    addIndent = map (\(a,b) -> [DrawText (tab <> "|- " <> toString (hLongText a) <> " ") defaultFont, DrawText (toString b)  redFont])

processTrtQurey :: (Text.Text, Update) -> IO [SendMsg]
processTrtQurey (cmdBody, update) = do
  let mode = Text.splitOn " " cmdBody
  case length mode of
    0 -> pure []
    1 ->
      if cmdBody == "!clear"
         then do
           removeDirectoryRecursive "wldata/Torrents" >> createDirectory "wldata/Torrents"
           pure [makeReqFromUpdate update "[信息] 缓存已清除。"]
         else do
           let imgCachePath = "images/TR-" <> Text.unpack cmdBody <> ".jpg"
           exist <- doesFileExist imgCachePath
           if not exist
              then do
                links' <- getTrtLinks defaultQueryCnt (Text.unpack cmdBody)
                case links' of
                  Just links -> do
                    infos <- getTrtInfos links
                    texts <- generateTrtTexts infos
                    drawTextArray imgCachePath 50 texts
                    BL.writeFile ("wldata/Torrents/TR-" <> Text.unpack cmdBody <> ".txt") $ encode $ fmap trt_mag infos
                  _ -> pure ()
              else pure ()
           exist' <- doesFileExist imgCachePath
           if not exist'
              then pure [makeReqFromUpdate update "[错误] 未找到磁链。"]
              else pure [makeReqFromUpdate'' update (drop 7 imgCachePath) ""]
    2 -> do
      let i = fromRight 0 $ fst <$> decimal (mode !! 1)
      if i > defaultQueryCnt
         then pure [makeReqFromUpdate update "[错误] 索引超出界限。"]
         else do
           let magCachePath = "wldata/Torrents/TR-" <> Text.unpack (head mode) <>".txt"
           exist <- doesFileExist magCachePath
           if not exist
              then pure [makeReqFromUpdate update "[错误] 本地无缓存。"]
              else do
                mags <- eitherDecode <$> BL.readFile magCachePath :: IO (Either String Array)
                case mags of
                  Left err -> pure [makeReqFromUpdate update $ "[错误] " <> Text.pack err]
                  Right lst -> pure [makeReqFromUpdate update $ "[磁链] " <> showStr (lst ! i)]
    _ -> pure [makeReqFromUpdate update "[错误] 参数错误。"]
    where
      showStr (String x) = x
      showStr _          = ""

type FileName = BL.ByteString
type FileSize = BL.ByteString
data TrtInfo = TrtInfo {
    trt_name  :: BL.ByteString
  , trt_date  :: BL.ByteString
  , trt_files :: [(FileName, FileSize)]
  , trt_mag   :: Text.Text
} deriving (Show)

defaultFont :: FontDescrb
defaultFont = FontDescrb FontBlack FZHeiTi
redFont :: FontDescrb
redFont = FontDescrb FontRed FZHeiTi

trtDrctRqmt :: [String]
trtDrctRqmt = ["wldata/Torrents"]
