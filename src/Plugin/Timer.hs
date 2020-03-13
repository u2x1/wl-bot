{-# LANGUAGE OverloadedStrings #-}
module Plugin.Timer where

import Core.Web.CoolQ               as CQ
import Core.Data.CoolQ               as CQ
import Core.Type.CoolQ.Update               as CQ

import Utils.Config
import Utils.Logging

import Data.Time
import Control.Concurrent
import qualified Data.Text as Text
import Data.Text.Read as Text
import Data.Text.IO as Text
import Data.Either
import Data.Maybe

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

processTimerOp :: Config -> CQ.Update -> IO (Maybe RespBS)
processTimerOp config cqUpdate =
  let msgTxt = getText (CQ.message cqUpdate)
      userId = CQ.user_id cqUpdate           in
    case Text.take 3 msgTxt of
      "/al" ->
        let content = Text.dropWhile (==' ') (Text.drop 3 msgTxt) in
          if content /= "" && isRight (decimal content)
             then do
               timers <- Text.readFile "timers.txt"
               let alarmTime = 60 * fromIntegral (fromRight 0 $ fst <$> decimal content::Int)
               timerState <- addTimer alarmTime (fromJust userId)
               case timerState of
                 Just _ -> logWT Info ("Pomodoro set from " <> show userId) >> Just <$> sendBackTextMsg (Text.pack $ show alarmTime <> " alarm is set up.") cqUpdate config
                 _      -> Just <$> sendBackTextMsg "You're having an on-going task." cqUpdate config
               logWT Info ("Alarm [" <> show alarmTime <>"] set from " <> show userId)
               Just <$> sendBackTextMsg "The alarm time is up." cqUpdate config
             else Just <$> sendBackTextMsg "Bad format.\nUsage: /al TIME\nTIME uses the unit of minutes." cqUpdate config
      "/tm" -> do
        timers <- Text.readFile "timers.txt"
        timerState <- addTimer 60 (fromJust userId)
        case timerState of
          Just _ -> logWT Info ("Pomodoro set from " <> show userId) >> Just <$> sendBackTextMsg "Pomodoro 25 minutes started." cqUpdate config
          _      -> Just <$> sendBackTextMsg "You're having an on-going task." cqUpdate config

      "/cl" -> do
        fileContent <- Text.readFile "timers.txt"
        let timers = Prelude.init $ Text.splitOn "\n" fileContent
        let afterRmTimers =
              filter (\timer -> snd (Text.breakOn (Text.pack $show$ fromJust userId) timer) == "") timers
        _ <- Text.writeFile "timers.txt" $ foldr (\a b -> a <> "\n" <> b) "" afterRmTimers
        Just <$> sendBackTextMsg "Alarm cancelled." cqUpdate config
      _ -> pure Nothing

rmTimeOutUser :: IO [Integer]
rmTimeOutUser = do
  fileContent <- Text.readFile "timers.txt"
  let timers = fmap (Text.breakOn " ") (Prelude.init $ Text.splitOn "\n" fileContent)
  outUsers <- traverse checkIfTimesOut timers
  let timeOutUsers = Text.pack.(" "++).show.fromJust <$> filter isJust outUsers
  let notTimeOutUsers = filter (\(_,userId) -> userId `notElem` timeOutUsers) timers
  _ <- Text.writeFile "timers.txt" $ foldr (\a b -> a <> "\n" <> b) "" (fmap mergeTuple notTimeOutUsers)
  pure $ fromJust $ sequence $ filter isJust outUsers
  where
    mergeTuple (a,b) = a <> b
    checkIfTimesOut ("",_) = pure Nothing
    checkIfTimesOut (time, userId) = do
      nowTime <- getCurrentTime
      if nowTime > readCusTime (Text.unpack time)
         then pure $ Just <$> fromRight 1 $ fst <$> decimal (Text.drop 1 userId)
         else pure Nothing
