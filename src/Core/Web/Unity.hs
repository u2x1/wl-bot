{-# LANGUAGE OverloadedStrings #-}
module Core.Web.Unity where

import Control.Lens
import Core.Type.Unity.Request    as U
import Core.Web.Telegram          as T
import Core.Web.Mirai             as Q
import Core.Data.Mirai            as Q
import Core.Type.Telegram.Request as T
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
      postTgRequest (config ^. tg_token) "sendMessage" $
        toJSON (T.SendMsg (U.chat_id msg) (fromMaybe "" $ U.text msg))

    -- Handle QQ message
    QQ ->
      case target_type msg of
        Private -> Q.sendPrivMsg (chat_id msg) (Q.transMsg msg) config (reply_id msg)
        Group   -> Q.sendGrpMsg  (chat_id msg) (Q.transMsg msg) config (reply_id msg)
