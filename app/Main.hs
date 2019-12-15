import Web.Scotty
import Network.HTTP.Types (status204)
import Network.Wai.Middleware.RequestLogger
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Exception (catch)
import Control.Concurrent
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy as B
import Data.Aeson (decode)
import Data.CQ
import Type.Telegram.Update as TG
import Type.CQ.Update       as CQ
import Type.Config
import Web.CQ
import Web.Telegram
import Utils.Config
import Utils.Logging

main = do
  c <- B.readFile "config.json" `catch` makeExampleConfig
  let config = fromMaybe exampleConfig (decode c :: Maybe Config)
  scotty (port config) $ do
--    middleware logStdoutDev
  
    post (literal "/telegram/") $ do
      update <- jsonData :: ActionM TG.Update
      _ <- liftIO $ forkFinally (fwdTgtoQQ (cqServer config) update (groups config)) handleExp
      status status204
  
    post (literal "/cq/") $ do
      update <- jsonData :: ActionM CQ.Update
      _ <- liftIO $ forkFinally (fwdQQtoTg (tgbotToken config) update (groups config)) handleExp
      status status204
    where
      handleExp (Right _) = logWT "Info" "Forwarded a message"
      handleExp (Left err)  = logWT "Error" ("Failed to forward a message: " <> show err)
