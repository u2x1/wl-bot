module Utils.Logging where

import Data.Time

logWT :: String -> String -> IO ()
logWT tag msg = do
  t <- getCurrentTime
  let nowtime = show $ formatTime defaultTimeLocale "%Y/%m/%d %H:%M" t
  putStrLn $ concat ["[", nowtime, "] ", "[", tag, "] ", msg]
