-- {-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils.ModuleHelper where

import           Control.Lens                   ( (^.) )
import           Data.Aeson                     ( FromJSON
                                                , decode
                                                )
import           Data.List                      ( intersperse )
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           GHC.Generics                   ( Generic )
import           Network.Wreq                  as Wreq
                                                ( FormParam((:=))
                                                , post
                                                , responseBody
                                                )

import           Core.Data.Unity                ( makeReqFromUpdate )
import           Core.Type.Unity.Request       as UR
                                                ( SendMsg(SendMsg) )
import           Core.Type.Unity.Update        as UU
                                                ( Update
                                                  ( chat_id
                                                  , message_type
                                                  , platform
                                                  )
                                                )
import           Core.Type.Universal            ( Platform(QQ, Telegram)
                                                , TargetType(Group, Private)
                                                )



-- getShortUrl :: Text -> IO (Maybe Text)
-- getShortUrl originUrl = do
--   rsp <- Wreq.post "https://29.pm/api.php" ["d" := originUrl]
--   let s = decode (rsp ^. responseBody) :: Maybe ShortURL
--   return (shorturl <$> s)

-- newtype ShortURL = ShortURL {
--    shorturl :: Text
-- } deriving (Generic, Show)
-- instance FromJSON ShortURL

rmSubscribe :: String -> FilePath -> (T.Text, Update) -> IO [SendMsg]
rmSubscribe plat fp (_, update) = do
  fileContent <- T.readFile fp
  if snd (T.breakOn (T.pack $ show (UU.chat_id update)) fileContent) == ""
    then pure [makeReqFromUpdate update ("您不在" <> T.pack plat <> "的订阅列表内。")]
    else do
      let subscribers  = T.splitOn "\n" fileContent
          afterRmUsers = filter
            (\user ->
              snd (T.breakOn (T.pack $ show (UU.chat_id update)) user) == ""
            )
            subscribers
      _ <- T.writeFile fp $ (mconcat . intersperse "\n") afterRmUsers
      pure [makeReqFromUpdate update ("取消对" <> T.pack plat <> "的订阅成功。")]

addSubscriber :: String -> FilePath -> (T.Text, Update) -> IO [SendMsg]
addSubscriber plat fp (_, update) = do
  ss <- T.readFile fp
  if snd (T.breakOn (T.pack $ show chatId) ss) == ""
    then do
      appendFile
        fp
        (  show chatId
        <> " "
        <> show (platform update)
        <> " "
        <> show (message_type update)
        <> "\n"
        )
      pure [makeReqFromUpdate update ("订阅[" <> T.pack plat <> "]成功。")]
    else pure [makeReqFromUpdate update ("您已在[" <> T.pack plat <> "]的订阅列表内。")]
  where chatId = UU.chat_id update


parseSubscriber :: FilePath -> IO (T.Text -> [SendMsg])
parseSubscriber fp = do
  fileContent <- T.readFile fp
  let subscribers = T.splitOn " " <$> T.splitOn "\n" fileContent
  let infos       = catMaybes $ getSubscriberInfos <$> subscribers
  pure $ traverse (uncurry3 SendMsg) infos . Just
 where
  uncurry3 f (a, b, c) = f a a b c Nothing Nothing Nothing
  getSubscriberInfos [userId, plat, targetType] = Just
    ( T.unpack userId
    , case targetType of
      "Private" -> Private
      "Group"   -> Group
      _         -> error "Unrecognized target type."
    , case plat of
      "Telegram" -> Telegram
      "QQ"       -> QQ
      _          -> error "Unrecognized platform."
    )
  getSubscriberInfos _ = Nothing
