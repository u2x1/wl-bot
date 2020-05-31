{-# LANGUAGE OverloadedStrings #-}
module Core.Data.MsgLog where

import Core.Type.Unity.Update
import Core.Type.Universal
import Data.Text                 (breakOn, unpack, pack, splitOn, Text)
import Data.Text.IO         as T (readFile)
import Data.Text.Read            (decimal)
import Data.Maybe                (fromJust)
import Utils.Logging

-- | Fetch a logged message from local file "wlMsg.log".
fetchMsg :: Integer -> IO (Maybe Update)
fetchMsg msgId = do
  msgLog <- T.readFile "wlMsg.log"
  case fst $ breakOn "\r\n" $ snd $ breakOn ("> " <> pack (show msgId) <> " ||") msgLog of
    "" -> return Nothing
    tMsg -> do
      let msgArr = splitOn " || " tMsg
      let update = Update QQ userId chatId msgTxt msgImgs Group msgId Nothing
          userId = (fromJust $ text2Int $ msgArr !! 1) :: Integer
          chatId = (fromJust $ text2Int $ msgArr !! 2) :: Integer
          msgTxt = Just $ unpack $ msgArr !! 3
          msgImgs = Just $ map unpack $ splitOn " " $ msgArr !! 4
      return $ Just update
      where
        text2Int txt = case decimal txt of
                         Right x -> Just $ fst x
                         _ -> Nothing
