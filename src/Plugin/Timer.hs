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
  timers <- Text.readFile "timers.txt"
  if snd (Text.breakOn (Text.pack $ show userId) timers) == ""
     then do
       nowTime <- getCurrentTime
       let recordTime = formatTime defaultTimeLocale timeFormat $ addUTCTime countdown nowTime
       _ <- Text.appendFile "timers.txt" $ Text.pack
              (recordTime <> " " <> show userId <> " " <> show plat <> "\n")
       pure $ Just ()
     else pure Nothing

checkTimer :: IO [SendMsg]
checkTimer = do
  usersWithPlat <- rmTimeOutUser
  pure $ (\(userId, plat) -> SendMsg "时间到。" userId Private plat) <$> usersWithPlat

rmTimeOutUser :: IO [(Integer, Platform)]
rmTimeOutUser = do
  fileContent <- Text.readFile "timers.txt"
  let timers = map (Text.splitOn " ") (Text.splitOn "\n" fileContent)
  timerList <- traverse checkIfTimesOut timers
  _ <- Text.writeFile "timers.txt" $ (mconcat.intersperse "\n") (lefts timerList)
  pure $ rights timerList
  where
    checkIfTimesOut [time, userId, plat] = do
      nowTime <- getCurrentTime
      if nowTime > readCusTime (Text.unpack time)
         then pure $ Right (fromRight 1 $ fst <$> decimal userId,
                                   case plat of
                                     "Telegram" -> Telegram
                                     "QQ" -> QQ
                                     _    -> error "Unrecognized platform.")
         else pure $ Left (time <> " " <> userId <> " " <> plat)
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
        else pure [makeReqFromUpdate update timerHelps]
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
  fileContent <- Text.readFile "timers.txt"
  let timers = Prelude.init $ Text.splitOn "\n" fileContent
      afterRmTimers =
        filter (\timer -> snd (Text.breakOn (Text.pack $ show (user_id update)) timer) == "") timers
  _ <- Text.writeFile "timers.txt" $ (mconcat.intersperse "\n") afterRmTimers
  pure [makeReqFromUpdate update "倒计时已取消。"]

timerHelps :: Text.Text
timerHelps = Text.unlines [ "====Timer===="
                          , "/timer TIME: 设定一个倒计时(以分钟为单位)"
                          , "/cxltimer: 取消已设定的倒计时"
                          , "/pd: 一个缩写版本的\"/timer 25\"(\"pd\"意为Pomodoro)"]
