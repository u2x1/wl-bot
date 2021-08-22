{-# LANGUAGE OverloadedStrings #-}
module Module.SauceNAOSearcher where

import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (^.)
                                                )
import           Control.Monad                  ( join )
import           Core.Data.Unity                ( makeReqFromUpdate' )
import           Core.Type.EitherT              ( MEitherT(runMEitherT)
                                                , getTextT'
                                                , lift
                                                , liftEither
                                                , liftList
                                                )
import           Core.Type.Unity.Request       as UR
                                                ( SendMsg )
import           Core.Type.Unity.Update        as UU
                                                ( Update(message_image_urls) )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(parseJSON)
                                                , eitherDecode
                                                , withObject
                                                )
import           Data.Either                    ( fromRight )
import           Data.Maybe                     ( catMaybes
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as T
import           Data.Text.Read                 ( decimal )
import           Module.Ascii2dSearcher         ( processAscii2dSearch )
import           Module.NHentaiQuerier          ( getNHentaiBookId )
import           Network.Wreq                  as Wreq
                                                ( defaults
                                                , getWith
                                                , param
                                                , responseBody
                                                )
-- import Utils.ModuleHelper ( getShortUrl )
import           Utils.Logging                  ( LogTag(Info)
                                                , logWT
                                                )
import           Utils.Misc                    as Misc
                                                ( unlines )

runSauceNAOSearch :: String -> String -> IO (Either T.Text SnaoResults)
runSauceNAOSearch apiKey imgUrl' = runMEitherT $ do
  r <- liftEither getErrHint $ try $ Wreq.getWith
    opts
    "https://saucenao.com/search.php"
  liftEither (pack . ("解析JSON错误: " <>))
             (pure $ eitherDecode $ r ^. responseBody)
 where
  opts =
    defaults
      &  param "db"
      .~ ["999"]
      &  param "output_type"
      .~ ["2"]
      &  param "numres"
      .~ ["1"]
      &  param "api_key"
      .~ [T.pack apiKey]
      &  param "url"
      .~ [T.pack imgUrl']

getErrHint :: SomeException -> Text
getErrHint excp = if snd (T.breakOn "rate limit" (T.pack $ show excp)) == ""
  then "请求错误: " <> T.pack (show excp)
  else "已超出30秒内搜图限制。"


processSnaoQuery :: (T.Text, Update) -> IO [SendMsg]
processSnaoQuery (_, update) = fmap (getTextT' update) $ runMEitherT $ do
  imgUrl'' <- liftList "无效图片。" $ pure $ message_image_urls update
  lift $ logWT Info $ "SauceNAO" <> show imgUrl''
  result <- liftEither id $ runSauceNAOSearch
    "d4c5f40172cb923c73c409538f979482a469d5a7"
    (head imgUrl'')
  sendMsgs <- lift $ traverse (getText update) (sr_results result)

  let highestSimilarity =
        fromRight 0 $ fst <$> decimal (sr_similarity (head $ sr_results result)) :: Int
  urlMsgs <- if highestSimilarity > 70
    then pure []
    else lift $ processAscii2dSearch (T.empty, update)

  pure $ urlMsgs <> catMaybes sendMsgs

getText :: Update -> SnaoResult -> IO (Maybe SendMsg)
getText update rst = do
  infos <- getInfo rst
  pure . Just $ makeReqFromUpdate' update (sr_thumbnail rst) $ Misc.unlines
    infos

getInfo :: SnaoResult -> IO [Text]
getInfo sRst = do
  let originUrl = head <$> sr_ext_url sRst
  -- shortenUrl <- join <$> traverse getShortUrl originUrl

  let similarity = pure $ sr_similarity sRst
      -- source     = if isJust shortenUrl then shortenUrl else originUrl
      source     = originUrl
      siteDomain =
        T.takeWhile (/= '/') . T.drop 2 . T.dropWhile (/= '/') <$> originUrl
      title     = sr_title sRst
      pixiv_mem = sr_pixiv_member sRst
      -- pixiv_id  = sr_pixiv_id sRst


  (doujinshi_name, link) <- do
    let dn = sr_doujinshi_name sRst
    n <- join <$> traverse getNHentaiBookId dn
    let nhentaiLink = ("https://nhentai.net/g/" <>) . snd <$> n
    pure (fst <$> n, nhentaiLink)

  pure
    .  catMaybes
    $  [Just "# SauceNAO"]
    <> mkInfo "相似度" similarity
    <> mkInfo "图源"  source
    <> mkInfo "域名"  siteDomain
    <> mkInfo "标题"  title
    <> mkInfo "画师"  pixiv_mem
    <> mkInfo "本子"  doujinshi_name
    <> mkInfo "链接"  link
  where mkInfo key value = (: []) $ (("[" <> key <> "] ") <>) <$> value

data SnaoResults = SnaoResults
  { sh_short_remaining :: Int
  , sh_long_remaining  :: Int
  , sh_status          :: Int
  , sr_results         :: [SnaoResult]
  }
  deriving Show
instance FromJSON SnaoResults where
  parseJSON = withObject "SnaoResults" $ \v ->
    SnaoResults
      <$> ((v .: "header") >>= (.: "short_remaining"))
      <*> ((v .: "header") >>= (.: "long_remaining"))
      <*> ((v .: "header") >>= (.: "status"))
      <*> (v .: "results")

data SnaoResult = SnaoResult
  { sr_similarity     :: T.Text
  , sr_thumbnail      :: T.Text
  , sr_ext_url        :: Maybe [T.Text]
  , sr_title          :: Maybe T.Text
  , sr_doujinshi_name :: Maybe T.Text
  , sr_pixiv_member   :: Maybe T.Text
  , sr_pixiv_id       :: Maybe Integer
  }
  deriving Show
instance FromJSON SnaoResult where
  parseJSON = withObject "SnaoResult" $ \v ->
    SnaoResult
      <$> ((v .: "header") >>= (.: "similarity"))
      <*> ((v .: "header") >>= (.: "thumbnail"))
      <*> ((v .: "data") >>= (.:? "ext_urls"))
      <*> ((v .: "data") >>= (.:? "title"))
      <*> ((v .: "data") >>= (.:? "jp_name"))
      <*> ((v .: "data") >>= (.:? "member_name"))
      <*> ((v .: "data") >>= (.:? "pixiv_id"))
