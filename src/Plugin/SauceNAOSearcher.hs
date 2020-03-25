{-# LANGUAGE OverloadedStrings #-}
module Plugin.SauceNAOSearcher where

import           Network.Wreq as Wreq
import           Control.Lens
import qualified Data.Text as Text
import           Utils.Misc                           (searchBetweenBL)
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as TextL
import           Core.Type.Unity.Update as UU
import           Core.Type.Unity.Request as UR
import           Utils.Logging
import           Core.Data.Unity

type Similarity = Text.Text
type Url        = Text.Text
runSauceNAOSearch :: String -> Text.Text -> IO (Maybe (Similarity, Url))
runSauceNAOSearch apiKey imgUrl = do
  r <- Wreq.get $
    "https://saucenao.com/search.php?db=999&output_type=2&numres=1&api_key=" <> apiKey <> "&url=" <> Text.unpack imgUrl
  let responseContent = r ^. responseBody
  let similarity = searchBetweenBL "\"similarity\":\"" "\"" responseContent
  let url = searchBetweenBL "\"ext_urls\":[\"" "\"" responseContent
  let infos = ((<$>).(<$>)) (TextL.toStrict.decodeUtf8) (sequence [similarity, url])
  pure $ (,) <$> ((!! 0) <$> infos) <*> (Text.replace "\\/" "/".(!! 1) <$> infos)

processSnaoQuery :: (Text.Text, Update) -> IO [SendMsg]
processSnaoQuery (_, update) =
   case message_image_urls update of
     Just imgUrls -> do
       result <- runSauceNAOSearch "" $ head imgUrls
       logWT Info $
         "SauceNAO Query: [" <> Text.unpack (head imgUrls) <> "] sending from " <> show (user_id update)
       case result of
         Just rst -> pure [makeReqFromUpdate update ("相似度: [" <> fst rst <> "]\n" <> snd rst)]
         _ -> pure [makeReqFromUpdate update "未找到近似图片。"]
     _ -> pure [makeReqFromUpdate update snaoHelps]

snaoHelps :: Text.Text
snaoHelps = Text.unlines [ "====SauceNAOSearcher===="
                         , "/sp<PIC>: 从SauceNAO搜索一张图片。"]

-- newtype SnaoResult = SnaoResult {
--     sr_header  :: SnaoHeader
-- } deriving (Show)
--
-- data SnaoHeader = SnaoHeader {
--     long_remaining :: Int
--   , short_remaining :: Int
--   , status :: Int
-- } deriving (Show)
