{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Core.Web.Unity where

import Control.Lens
import Control.Exception
import Core.Type.Unity.Request    as U
import Core.Web.Telegram          as T
import Core.Data.Telegram         as T
import Core.Web.Mirai             as Q
import Core.Data.Mirai            as Q
import Network.Wreq
import Data.ByteString.Lazy
import Data.Aeson
import Utils.Config
import Data.Maybe

import Core.Type.Universal

type RB = Response ByteString

sendTextMsg :: U.SendMsg -> Config -> IO (Response ByteString)
sendTextMsg m c = do
  s <- try (retry3Times (sendTextMsg' m c)) :: IO (Either SomeException RB)
  case s of
    Right x -> return x
    Left _  ->
      let msg = (set imgUrl Nothing) . (set imgPath Nothing) . (set text (Just "Mirai boomed")) $ m in
      retry3Times (sendTextMsg' msg c)

sendTextMsg' :: U.SendMsg -> Config -> IO (Response ByteString)
sendTextMsg' msg config =
  case msg ^. target_plat of
    Telegram ->
      case T.transMsg msg of
        Left p -> postTgRequest' (config ^. tg_token) "sendPhoto" p
        Right s -> postTgRequest (config ^. tg_token)
          (if isJust $ msg ^. imgUrl then "sendPhoto" else "sendMessage") $
            toJSON s
    QQ ->
      case msg ^. target_type of
        Private -> Q.sendPrivMsg (msg ^. chat_id) (Q.transMsg msg) config
        Temp    -> Q.sendTempMsg (msg ^. user_id) (msg ^. chat_id) (Q.transMsg msg) config
        Group   -> Q.sendGrpMsg  (msg ^. chat_id) (Q.transMsg msg) config (msg ^. reply_id)

retry3Times :: IO a -> IO a
retry3Times action = (catch' . catch' . catch') action
  where catch' a = catch action (\e -> const a (e :: SomeException))
