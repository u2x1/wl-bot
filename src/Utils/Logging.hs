module Utils.Logging where

import Data.Time
import Control.Concurrent
import System.IO

data LogTag = Info | Warning | Error | Debug

-- Log with tag
logWT :: LogTag -> String -> IO ()
logWT logTag msg = do
  _ <- forkIO $ do
    t <- getCurrentTime
    let nowtime = formatTime defaultTimeLocale "%Y/%m/%d %H:%M" t
    putStrLn $ concat ["[", nowtime, "] ", "[", transTag logTag, "] ", msg]
    hFlush stdout
  pure ()

transTag :: LogTag -> String
transTag tag =
  case tag of
    Info    -> "Info"
    Warning -> "Warning"
    Error   -> "Error"
    Debug   -> "Debug"

logErr :: String -> String -> IO ()
logErr errText msg = logWT Error (msg ++ ": " ++ errText)
