{-# LANGUAGE OverloadedStrings #-}
module Module.SauceNAOSearcher where

import           Network.Wreq as Wreq
import           Control.Lens
import           Data.Text    as Text (Text, pack, unpack, breakOn)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Text.Lazy          (toStrict)
import           Data.ByteString.Lazy (ByteString)
import           Data.Aeson
import           Control.Exception
import           Core.Type.Unity.Update  as UU
import           Core.Type.Unity.Request as UR
import           Utils.Misc as Misc
import           Core.Data.Unity
import           Core.Type.EitherT
import           Data.Maybe

import           Module.NHentaiQuerier

runSauceNAOSearch :: String -> String -> IO (Either Text.Text SnaoResults)
runSauceNAOSearch apiKey imgUrl' = runMEitherT $ do
  r <- liftEither getErrHint $ try $ Wreq.getWith opts "https://saucenao.com/search.php"
  liftEither (pack.("解析JSON错误: "<>)) (pure $ eitherDecode $ r ^. responseBody)
  where opts = defaults & param "db" .~ ["999"]
                        & param "output_type" .~ ["2"]
                        & param "numres" .~ ["1"]
                        & param "api_key" .~ [Text.pack apiKey]
                        & param "url" .~ [Text.pack imgUrl']

getErrHint :: SomeException -> Text
getErrHint excp =
      if snd (Text.breakOn "rate limit" (Text.pack $ show excp)) == ""
         then "请求错误: " <> Text.pack (show excp)
         else "已超出30秒内搜图限制。"


processSnaoQuery :: (Text.Text, Update) -> IO [SendMsg]
processSnaoQuery (_, update) =
  fmap (getTextT' update) $
    runMEitherT $ do
      imgUrl' <- liftMaybe "无效图片。" $ pure $ head <$> message_image_urls update
      result <- liftEither id $ runSauceNAOSearch "d4c5f40172cb923c73c409538f979482a469d5a7" imgUrl'
      sendMsgs <- lift $ traverse (getText update) (sr_results result)
      pure $ catMaybes sendMsgs

getText :: Update -> SnaoResult -> IO (Maybe SendMsg)
getText update rst =
  case head <$> sr_ext_url rst of
    Just extUrl -> do
      pixivInfo <-
        if snd (breakOn "pixiv" extUrl) == ""
           then pure Nothing
           else do
             r' <- try (get (unpack extUrl)) :: IO (Either SomeException (Response ByteString))
             case r' of
               Right r ->
                 let authorId = decodeUtf8 <$>searchBetweenBL "userId\":\"" "\"" (r ^. responseBody)
                     authorName = decodeUtf8 <$> searchBetweenBL "userName\":\"" "\"" (r ^. responseBody) in
                 return $ (\x y -> ["[画师] " <> x] <> ["[画师ID] " <> y]) <$> authorName <*> authorId
               Left _ -> pure Nothing
      pure $ Just $ makeReqFromUpdate' update (sr_thumbnail rst) $ Misc.unlines $
                                               [ "[相似度] " <> sr_similarity rst
                                               , "[图源] " <> extUrl] <>
                                                 case pixivInfo of
                                                   Just x -> toStrict <$> x
                                                   Nothing -> []
    _ ->  maybe' (sr_doujinshi_name rst) (pure Nothing) (\dn -> do
          n <- getNHentaiBookId dn
          pure $ Just $ makeReqFromUpdate update $ Misc.unlines $ ["[本子名] " <> dn] <>
            [maybe' n "" (("[链接] https://nhentai.net/g/" <>).snd)])

data SnaoResults = SnaoResults {
    sh_short_remaining :: Int
  , sh_long_remaining :: Int
  , sh_status :: Int
  , sr_results :: [SnaoResult]
} deriving (Show)
instance FromJSON SnaoResults where
  parseJSON = withObject "SnaoResults" $ \v -> SnaoResults
        <$> ((v .: "header") >>= (.: "short_remaining"))
        <*> ((v .: "header") >>= (.: "long_remaining"))
        <*> ((v .: "header") >>= (.: "status"))
        <*> (v .: "results")

data SnaoResult = SnaoResult {
    sr_similarity :: Text.Text
  , sr_thumbnail :: Text.Text
  , sr_ext_url :: Maybe [Text.Text]
  , sr_doujinshi_name :: Maybe Text.Text
} deriving (Show)
instance FromJSON SnaoResult where
  parseJSON = withObject "SnaoResult" $ \v -> SnaoResult
        <$> ((v .: "header") >>= (.: "similarity"))
        <*> ((v .: "header") >>= (.: "thumbnail"))
        <*> ((v .: "data") >>= (.:? "ext_urls"))
        <*> ((v .: "data") >>= (.:? "jp_name"))
