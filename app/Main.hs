import Web.Scotty
import Network.HTTP.Types                    (status204)
import Network.Wai.Middleware.RequestLogger
import Control.Monad.IO.Class                (liftIO)
import Control.Concurrent.Lock
import Control.Exception                     (try, SomeException)
import Control.Concurrent                    (forkFinally)
import Data.Maybe                            (fromMaybe)
import Data.ByteString.Lazy as B
import Data.Aeson                            (decode)
import Type.Telegram.Update as TG
import Type.CoolQ.Update    as CQ
import Type.Config
import Web.CoolQ
import Web.Telegram
import Utils.Config
import Utils.Logging

main = do
  cb <- try $ B.readFile "config.json" :: IO (Either SomeException ByteString)
  case cb of
    Right c -> runServer $ fromMaybe exampleConfig (decode c :: Maybe Config)
    Left err -> logWT "Error" ("Failed to open config file: " <> show err)

runServer config = do
  tgLock <- new :: IO Lock
  cqLock <- new :: IO Lock
  scotty (port config) $ do
    middleware logStdoutDev
    post (literal "/telegram/") $ do
      update <- jsonData :: ActionM TG.Update
      liftIO $ fwdTGtoQQ cqLock (cqServer config) update (groups config)
      status status204
    post (literal "/cq/") $ do
      update <- jsonData :: ActionM CQ.Update
      liftIO $ fwdQQtoTG tgLock (tgbotToken config) update (groups config)
      status status204
    where
      handleExp _ = pure ()
