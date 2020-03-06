module Utils.Webhook where

import Network.Wreq
import Data.ByteString.Lazy
import Utils.Config

setTelegramWebhook :: Config -> IO (Response ByteString)
setTelegramWebhook config = get target
  where target = Prelude.concat ["https://api.telegram.org/bot", token, "/setWebhook?url=", server,":", show wPort, "/telegram/"]
        server = thisServer config
        token  = tgbotToken config
        wPort   = port config
