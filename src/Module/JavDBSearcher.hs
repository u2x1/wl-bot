{-# LANGUAGE OverloadedStrings #-}
module Module.JavDBSearcher where

import Network.Wreq
import qualified Data.ByteString.Lazy as BL
import Utils.Misc as Misc
import qualified Data.Text as Text
import Core.Type.Unity.Request
import Core.Type.Unity.Update
import Core.Data.Unity
import Utils.Logging
import Data.Text.Encoding
import Data.ByteString.Lazy.UTF8 as UTF8
import Control.Lens

getFstUrl :: BL.ByteString -> Maybe String
getFstUrl content = fixUrl $ UTF8.toString <$> Misc.searchBetweenBL "href=\"/v/" "\"" (BL.drop 20000 content)
  where fixUrl = fmap ("https://javdb4.com/v/" <>)

getMagnet :: BL.ByteString -> Maybe Text.Text
getMagnet content = fixUrl $ (decodeUtf8 . BL.toStrict) <$> Misc.searchBetweenBL "magnet:?xt=" "\"" (BL.drop 500 content)
  where fixUrl = fmap ("magnet:?xt=" <>)

runJavDBSearch :: Text.Text -> IO (Maybe Text.Text)
runJavDBSearch query = do
  result <- getWith opts $ "https://javdb4.com/search"
  case getFstUrl (result ^. responseBody) of
    Nothing      -> pure Nothing
    Just realUrl -> do
      realRsp <- get realUrl
      case getMagnet $ realRsp ^. responseBody of
        Nothing -> pure Nothing
        Just magnet -> pure $ Just magnet
    where opts = defaults & param "q" .~ [query]

processJavDBQuery :: (Text.Text, Update) -> IO [SendMsg]
processJavDBQuery (cmdBody, update) =
  if content /= ""
     then do
       result <- runJavDBSearch content
       logWT Info $
         "Query: [" <> Text.unpack content <> "] sending from " <> show (user_id update)
       case result of
         Just r -> pure [makeReqFromUpdate update $ "[磁链] " <> r]
         _ -> pure [makeReqFromUpdate update "无结果。"]
     else pure []
    where
      content = Text.strip cmdBody
