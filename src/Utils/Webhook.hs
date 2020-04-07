module Utils.Webhook where

import Control.Lens
import Network.Wreq
import Utils.Config
import Control.Concurrent

setTelegramWebhook :: Config -> IO ThreadId
setTelegramWebhook config = forkFinally (get target) (pure $ pure ())
  where target = Prelude.concat ["https://api.telegram.org/bot", token, "/setWebhook?url=", server,":", show wPort, "telegram/"]
        server = config ^. webhook_server
        token  = config ^. tg_token
        wPort  = config ^. port
