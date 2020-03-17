{-# LANGUAGE OverloadedStrings #-}
module Plugin.Timer where

import           Core.Web.CoolQ                as CQ
import           Core.Type.Unity.Update
import           Utils.Config
import           Utils.Logging

import           Data.Time
import           Control.Concurrent
import qualified Data.Text                     as Text
import           Data.Text.Read                as Text
import           Data.Text.IO                  as Text
import           Data.Either
import           Data.List
import qualified Data.ByteString.Lazy          as BL
import           Network.Wreq

timeFormat :: String
timeFormat = "%Y/%m/%d-%H:%M"

readCusTime :: String -> UTCTime
readCusTime = parseTimeOrError True defaultTimeLocale timeFormat

addTimer :: NominalDiffTime -> Integer -> IO (Maybe ())
addTimer countdown userId = do
  timers <- Text.readFile "timers.txt"
  if snd (Text.breakOn (Text.pack $ show userId) timers) == ""
     then do
       nowTime <- getCurrentTime
       let recordTime = Text.pack $ formatTime defaultTimeLocale timeFormat $ addUTCTime countdown nowTime
       _ <- Text.appendFile "timers.txt" (recordTime <> " " <> Text.pack (show userId) <> "\n")
       pure $ Just ()
     else pure Nothing

checkTimer :: Config -> IO ()
checkTimer config = do
  users <- rmTimeOutUser
  _ <- traverse (\userId -> sendPrivMsg userId "Time's up." config) users
  threadDelay 60000000 >> checkTimer config

rmTimeOutUser :: IO [Integer]
rmTimeOutUser = do
  fileContent <- Text.readFile "timers.txt"
  let timers = map (Text.breakOn " ") (init $ Text.splitOn "\n" fileContent)
  timerList <- traverse checkIfTimesOut timers
  _ <- Text.writeFile "timers.txt" $ (mconcat.intersperse "\n") (lefts timerList)
  pure $ rights timerList
  where
    checkIfTimesOut ("",_) = pure $ Left ""
    checkIfTimesOut (time, userId) = do
      nowTime <- getCurrentTime
      if nowTime > readCusTime (Text.unpack time)
         then pure $ Right $ fromRight 1 $ fst <$> decimal (Text.drop 1 userId)
         else pure $ Left (time<>userId)

processTimerOp :: Config -> Update -> IO (Maybe (Response BL.ByteString))
processTimerOp config cqUpdate =
  let msgTxt = message_text cqUpdate
      userId = user_id cqUpdate      in
  case Text.take 3 msgTxt of
    "/al" ->
      if content /= "" && isRight (decimal content)
        then do
          let alarmTime = 60 * fromIntegral (fromRight 0 $ fst <$> decimal content::Int)
          timerState <- addTimer alarmTime userId
          case timerState of
            Just _ -> logWT Info ("Alarm ["<>show alarmTime<>"] set from " <> show userId) >>
              Just <$> sendBackTextMsg (Text.pack$show alarmTime<>" alarm is set up.") cqUpdate config
            _      -> Just <$> sendBackTextMsg "You're having an on-going task." cqUpdate config
        else Just <$> sendBackTextMsg
          "Bad format.\nUsage: /al TIME\nTIME uses the unit of minutes." cqUpdate config
      where
        content = Text.dropWhile (==' ') (Text.drop 3 msgTxt)

    "/pd" -> do
      timerState <- addTimer (60*25) userId
      case timerState of
        Just _ -> logWT Info ("Pomodoro set from " <> show userId) >>
                  Just <$> sendBackTextMsg "Pomodoro 25 minutes started." cqUpdate config
        _      -> Just <$> sendBackTextMsg "You're having an on-going task." cqUpdate config

    "/cl" -> do
      fileContent <- Text.readFile "timers.txt"
      let timers = Prelude.init $ Text.splitOn "\n" fileContent
      let afterRmTimers =
            filter (\timer -> snd (Text.breakOn (Text.pack $ show userId) timer) == "") timers
      _ <- Text.writeFile "timers.txt" $ foldr (\a b -> a <> "\n" <> b) "" afterRmTimers
      Just <$> sendBackTextMsg "Alarm cancelled." cqUpdate config

    _ -> pure Nothing
