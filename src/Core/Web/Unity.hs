module Core.Web.Unity where

import Core.Type.Unity.Request    as U
import Core.Web.Telegram          as T
import Core.Web.CoolQ             as Q
import Core.Type.Telegram.Request as T
import Network.Wreq
import Data.ByteString.Lazy
import Data.Aeson
import Utils.Config

import Core.Type.Universal

sendTextMsg :: U.SendMsg -> Config -> IO (Response ByteString)
sendTextMsg msg config =
  case target_plat msg of

    -- Handle Telegram message
    Telegram ->
      postTgRequest (tgbotToken config) "sendMessage" $
        toJSON (T.SendMsg (U.chat_id msg) (U.text msg))

    -- Handle QQ message
    QQ ->
      case msg_type msg of
        Private -> Q.sendPrivMsg (chat_id msg) (U.text msg) config
        Group   -> Q.sendGrpMsg  (chat_id msg) (U.text msg) config
