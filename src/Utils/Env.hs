module Utils.Env where

import qualified Network.WebSockets            as WS
import           Utils.Config                   ( Config )

data Env = Env
  { cfg    :: Config
  , wsConn :: WS.Connection
  }
