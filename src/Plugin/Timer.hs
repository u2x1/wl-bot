{-# LANGUAGE OverloadedStrings #-}
module Plugin.Timer where

import           Core.Type.Unity.Update        as UU
import           Core.Type.Unity.Request       as UR
import           Core.Data.Unity               as UN
import           Utils.Logging

import           Data.Time
import qualified Data.Text                     as Text
import           Data.Text.Read                as Text
import           Data.Text.IO                  as Text
import           Data.Either
import           Data.List
import           Core.Type.Universal

timeFormat :: String
timeFormat = "%Y/%m/%d-%H:%M"

readCusTime :: String -> UTCTime
readCusTime = parseTimeOrError True defaultTimeLocale timeFormat

saveTimer2File :: NominalDiffTime -> Integer -> Platform -> IO (Maybe ())
saveTimer2File countdown userId plat = do
  timers <- Text.readFile (head timerRqmt)
  if snd (Text.breakOn (Text.pack $ show userId) timers) == ""
     then do
       nowTime <- getCurrentTime
       let recordTime = formatTime defaultTimeLocale timeFormat $ addUTCTime countdown nowTime
       _ <- Text.appendFile (head timerRqmt) $ Text.pack
              (recordTime <> " " <> show userId <> " " <> show plat <> "\n")
       pure $ Just ()
     else pure Nothing

checkTimer :: IO [SendMsg]
checkTimer = do
  usersWithPlat <- rmTimeOutUser
  pure $ (\(userId, plat) -> SendMsg userId Private plat "时间到。") <$> usersWithPlat

rmTimeOutUser :: IO [(Integer, Platform)]
rmTimeOutUser = do
  fileContent <- Text.readFile (head timerRqmt)
  let timers = Text.splitOn " " <$> Text.splitOn "\n" fileContent
  timerList <- traverse checkIfTimesOut timers
  _ <- Text.writeFile (head timerRqmt) $ (mconcat.intersperse "\n") (lefts timerList)
  pure $ rights timerList
  where
    checkIfTimesOut [time, userId, plat] = do
      nowTime <- getCurrentTime
      if nowTime > readCusTime (Text.unpack time)
         then pure $ Right (fromRight 0 $ fst <$> decimal userId,
                                   case plat of
                                     "Telegram" -> Telegram
                                     "QQ" -> QQ
                                     _    -> error "Unrecognized platform.")
         else pure $ Left $ (mconcat.intersperse " ") [time, userId, plat]
    checkIfTimesOut _ = pure $ Left ""

addTimer :: (Text.Text, Update) -> IO [SendMsg]
addTimer (cmdBody, update) =
   if content /= "" && isRight (decimal content :: Either String (Int, Text.Text))
        then do
          let alarmTime = 60 * fromIntegral (fromRight 0 $ fst <$> decimal content :: Int)
          timerState <- saveTimer2File alarmTime (user_id update) (platform update)
          case timerState of
            Just _ -> logWT Info ("Timer ["<>show alarmTime<>"] set from " <> show (user_id update)) >>
              pure [makeReqFromUpdate update (Text.pack$show alarmTime<>"倒计时开始。")]
            _      -> pure [makeReqFromUpdate update "您有一个已设定的倒计时"]
        else pure []
      where
        content = Text.strip cmdBody

setPomodoro :: (Text.Text, Update) -> IO [SendMsg]
setPomodoro (_, update) = do
      timerState <- saveTimer2File (60*25) (user_id update) (platform update)
      case timerState of
        Just _ -> logWT Info ("Pomodoro set from " <> show (user_id update)) >>
                  pure [makeReqFromUpdate update "25分钟的番茄钟已开始。"]
        _      -> pure [makeReqFromUpdate update "你有一个已设定的倒计时。"]

cancelTimer :: (Text.Text, Update) -> IO [SendMsg]
cancelTimer (_, update) = do
  fileContent <- Text.readFile (head timerRqmt)
  let timers = Prelude.init $ Text.splitOn "\n" fileContent
      afterRmTimers =
        filter (\timer -> snd (Text.breakOn (Text.pack $ show (user_id update)) timer) == "") timers
  _ <- Text.writeFile (head timerRqmt) $ (mconcat.intersperse "\n") afterRmTimers
  pure [makeReqFromUpdate update "倒计时已取消。"]

timerRqmt :: [String]
timerRqmt = fmap ("wldata/" <>) ["TR-timers.txt"]
