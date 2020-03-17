{-# LANGUAGE OverloadedStrings #-}
module Plugin.BaikeSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.IO.Class
import Control.Monad.Trans.Identity

import Plugin.BaikeQuerier

spec :: Spec
spec =
  describe "runBaiduSearch" $
    it "should return a non-empty bytestring" $ do
      property $ \x -> liftIO $ do
        result <- runBaiduSearch x
        pure $ result /= ""
