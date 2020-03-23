{-# LANGUAGE OverloadedStrings #-}
module Plugin.DiceHelper where

import qualified Data.Text as Text
import System.Random
import Control.Monad
import Core.Type.Unity.Request
import Core.Type.Unity.Update
import Core.Data.Unity

import Utils.Logging

parseDice :: Text.Text -> Maybe (Int, Int)
parseDice rawMsg =
  case Text.breakOn "d" $ Text.toLower rawMsg of
    (_, "") -> Nothing
    ("", diceType) ->
      Just (1, read (Text.unpack $ Text.tail diceType) :: Int)
    (diceCnt, diceType) ->
      Just (read $ Text.unpack diceCnt :: Int, read (Text.unpack $ Text.tail diceType) :: Int)

rollDice :: (Int, Int) -> IO [Int]
rollDice (dcCnt,dcType) = do
  dices <- replicateM dcCnt randomIO :: IO [Int]
  pure $ map (`mod` (dcType+1)) dices

processDiceRolling :: (Text.Text, Update) -> IO [SendMsg]
processDiceRolling (cmdBody, update) =
  case parseDice $ Text.strip cmdBody of
    Just diceTup ->
      if fst diceTup > 100 || (snd diceTup * fst diceTup > 999999)
         then pure [makeReqFromUpdate update "骰子数或骰子面数超出限制。"]
         else do
           dice <- rollDice diceTup
           _ <- logWT Info $ "Dice " <> show diceTup <> " generated from " <> show (user_id update)
           pure [makeReqFromUpdate update $ Text.pack
             ("<" <> show (fst diceTup) <> "D" <> show (snd diceTup) <> "> " <> show (sum dice) <> if fst diceTup > 1 then "\n骰子依次为: " <> show dice else "")]
    Nothing -> pure [makeReqFromUpdate update diceHelps]

diceHelps :: Text.Text
diceHelps = Text.unlines [ "====DiceHelper===="
                         , "/dc DICE: DICE表示骰子类型以及数目。(/dc d6是一个六面骰子；/dc 2d12是两个十二面骰子)"
                         ]
