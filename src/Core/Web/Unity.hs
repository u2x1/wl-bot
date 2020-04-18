{-# LANGUAGE OverloadedStrings #-}
module Core.Web.Unity where

import Control.Lens
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

sendTextMsg :: U.SendMsg -> Config -> IO (Response ByteString)
sendTextMsg msg config =
  case target_plat msg of

    -- Handle Telegram message
    Telegram ->
      case T.transMsg msg of
        Left p -> postTgRequest' (config ^. tg_token) "sendPhoto" p
        Right s -> postTgRequest (config ^. tg_token)
          (if isJust $ imgUrl msg then "sendPhoto" else "sendMessage") $
            toJSON s

    -- Handle QQ message
    QQ ->
      case target_type msg of
        Private -> Q.sendPrivMsg (chat_id msg) (Q.transMsg msg) config (reply_id msg)
        Group   -> Q.sendGrpMsg  (chat_id msg) (Q.transMsg msg) config (reply_id msg)
