module Utils.Webhook where

import Control.Lens
import Network.Wreq
import Data.ByteString.Lazy
import Utils.Config

setTelegramWebhook :: Config -> IO (Response ByteString)
setTelegramWebhook config = get target
  where target = Prelude.concat ["https://api.telegram.org/bot", token, "/setWebhook?url=", server,":", show wPort, "/telegram/"]
        server =  config ^. webhook_server
        token  =  config ^. tg_token
        wPort  = config ^. port
