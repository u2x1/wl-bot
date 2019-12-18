import Web.Scotty
import Network.HTTP.Types (status204)
import Network.Wai.Middleware.RequestLogger
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import Control.Concurrent
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy as B
import Data.Aeson (decode)
import Data.CQ
import Data.Telegram
import Type.Telegram.Update as TG
import Type.CQ.Update       as CQ
import Type.Config
import Web.CQ
import Web.Telegram
import Utils.Config
import Utils.Logging

main = do
  cb <- try $ B.readFile "config.json" :: IO (Either SomeException ByteString)
  case cb of
    Right c ->
      let config = fromMaybe exampleConfig (decode c :: Maybe Config) in
      scotty (port config) $ do
--        middleware logStdoutDev
        post (literal "/telegram/") $ do
          update <- jsonData :: ActionM TG.Update
          _ <- liftIO $ forkFinally (fwdTgtoQQ (cqServer config) update (groups config)) handleExp
          status status204
      
        post (literal "/cq/") $ do
          update <- jsonData :: ActionM CQ.Update
          _ <- liftIO $ forkFinally (fwdQQtoTg (tgbotToken config) update (groups config)) handleExp
          status status204
    Left err -> logWT "Error" ("Failed to open config file: " <> show err)
    where
      handleExp (Right _) = logWT "Info" "Forwarded a message"
      handleExp (Left err)  = logWT "Error" ("Failed to forward a message: " <> show err)
