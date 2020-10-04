module Utils.Logging where

import           Data.Time
import           System.IO

data LogTag = Info | Warning | Error | Debug

-- Log with tag
logWT :: LogTag -> String -> IO ()
logWT logTag msg = do
  t <- getCurrentTime
  let nowtime = formatTime defaultTimeLocale "%Y/%m/%d %H:%M" t
  putStrLn $ concat ["[", nowtime, "] ", "[", transTag logTag, "] ", msg]
  hFlush stdout

transTag :: LogTag -> String
transTag tag =
  case tag of
    Info    -> "Info"
    Warning -> "Warning"
    Error   -> "Error"
    Debug   -> "Debug"

-- | Log errors with action name and details.
logErr :: String -> String -> IO ()
logErr msg errText = logWT Error (msg ++ ": " ++ errText)
