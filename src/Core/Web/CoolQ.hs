{-# LANGUAGE OverloadedStrings #-}
module Core.Web.CoolQ where

import Network.Wreq
import Data.Aeson
import Data.ByteString.Lazy
import Control.Concurrent
import Control.Concurrent.Lock
import Control.Exception.Base

postCqRequest :: Lock -> String -> String -> Value -> IO ThreadId
postCqRequest = ((.).(.).(.).(.)) forkIO postCqRequestSync

postCqRequestSync :: Lock -> String -> String -> Value -> IO ()
postCqRequestSync lock cqServer method jsonContent = do
  acquire lock
  try $ post target jsonContent :: IO (Either SomeException (Response ByteString))
  release lock
    where
      target = cqServer ++ "/" ++ method
