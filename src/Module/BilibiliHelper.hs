{-# LANGUAGE OverloadedStrings #-}
module Module.BilibiliHelper where

import Data.Bits       (xor)
import Data.Map.Strict ((!), fromList)
import Data.Maybe
import Data.Aeson
import Core.Type.Unity.Request
import Core.Type.Unity.Update
import Core.Data.Unity
import Utils.Logging
import Control.Lens
import qualified Data.Text as Text
import Data.Aeson.Types
import Network.Wreq
import Utils.Misc as Misc
import Data.Foldable

getVideoUrl :: Text.Text -> IO (Maybe Text.Text)
getVideoUrl inputId = do
  let vId = if (snd $ Text.breakOn "BV" inputId) == "" then inputId else (Text.pack.show.bv2av.Text.unpack) inputId
  cidM <- aid2Cid vId
  let opts = defaults & header "Cookie" .~ [""]
                      & param "avid" .~ [vId]
                      & param "cid"  .~ [fromMaybe "" cidM]
                      & param "fnval" .~ ["16"]
                      & param "fnver" .~ ["0"]

  r <- getWith opts biliApi
  either' (eitherDecode (r ^. responseBody))
    (\err -> (logErr "Decoding fstRst" err) >> (pure Nothing)) $ \result ->
      pure $ Just $ Text.pack $ head $ bl_video_urls result
  where
    biliApi = "https://api.bilibili.com/x/player/playurl"

processBiliQuery :: (Text.Text, Update) -> IO [SendMsg]
processBiliQuery (cmdBody, update) =
  if Text.null cmdBody
     then pure []
     else do
       logWT Info $
         "Bilibili query: [" <> Text.unpack cmdBody <> "] sending from " <> show (user_id update)
       let aid = Text.pack $ show $ bv2av $ Text.unpack cmdBody
       r <- getVideoUrl $ aid
       maybe' r (pure [makeReqFromUpdate update "无结果。"]) $ \result ->
         let msg = Misc.unlines ["[链接] " <> result, "[Referer] https://www.bilibili.com/av" <> aid] in
         pure [makeReqFromUpdate update msg]

aid2Cid :: Text.Text -> IO (Maybe Text.Text)
aid2Cid aid = do
  let opts = defaults & param "aid" .~ [aid]
  r <- getWith opts $ "https://www.bilibili.com/widget/getPageList"
  pure $ getCid $ r ^. responseBody
    where
      getCid rawJson = do
        result <- decode rawJson
        flip parseMaybe result $ \list ->
          ((.: "cid") =<< (parseJSON $ head list) :: Parser Int) >>= (pure.Text.pack.show)

data BlResult = BlResult {
    bl_quality :: Int
  , bl_accepted_quality :: [Int]
  , bl_video_urls :: [String]
} deriving (Show)
instance FromJSON BlResult where
  parseJSON = withObject "BlResult" $ \v -> BlResult
    <$> ((v .: "data") >>= (.: "quality"))
    <*> ((v .: "data") >>= (.: "accept_quality"))
    <*> (((v .: "data" >>= (.: "dash") >>= (.: "video")) :: Parser Array) >>=
          traverse parseJSON . toList >>=
            traverse (.: "base_url"))

bv2av :: String -> Int
bv2av str = if (Text.null.snd.Text.breakOn "BV") (Text.pack str)
               then read str
               else ((go str 0 0) - add) `xor` xorN
  where
    go _ r 6 = r
    go ori r cnt = go ori (r + (((58 ^ cnt) * (tr ! (ori !! (s !! cnt)))))) (cnt+1)

    tr = fromList $ trm table 0

    trm [] _ = []
    trm (x:xs) cnt = (x, cnt) : (trm xs (cnt+1))

    table  = "fZodR9XQDSUm21yCkr6zBqiveYah8bt4xsWpHnJE7jL5VG3guMTKNPAwcF"
    s      = [11,10,3,8,4,6]
    xorN   = 177451812
    add    = 8728348608
