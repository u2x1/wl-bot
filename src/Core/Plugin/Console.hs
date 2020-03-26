{-# LANGUAGE OverloadedStrings #-}
module Core.Plugin.Console where

import qualified Data.Text as Text
import           Data.List
import           Data.Foldable

import           System.Directory

import           Core.Type.Unity.Update
import           Core.Type.Unity.Request
import           Core.Web.Unity
import           Core.Data.Unity

import           Control.Concurrent
import           Control.Monad

import           Utils.Config

import           Plugin.BaikeQuerier
import           Plugin.NoteSaver
import           Plugin.Timer
import           Plugin.DiceHelper
import           Plugin.SolidotFetcher
import           Plugin.SauceNAOSearcher
import           Plugin.Ascii2dSearcher

getHandler :: Text.Text -> ((Text.Text, Update) -> IO [SendMsg])
getHandler cmdHeader =
  case Text.toLower $ Text.drop 1 cmdHeader of
    "bk" -> processQuery

    "svnote" -> saveNote
    "note" -> queryNote

    "timer" -> addTimer
    "cxltimer" -> cancelTimer
    "pd" -> setPomodoro

    "dc" -> processDiceRolling

    "subsd" -> addSubscriber
    "cxlsubsd" -> rmSubscribe

    "sp" -> processSnaoQuery
    "asc" -> processAscii2dSearch

    "help" -> getCommandHelps
    _     -> pure $ pure []

getMsgs2Send :: Update -> IO [SendMsg]
getMsgs2Send update =
  case message_text update of
    Just msgTxt ->
      if Text.head msgTxt == '/' || Text.head msgTxt == '.' || Text.head msgTxt == '。'
         then
           let command = Text.breakOn " " msgTxt in
           getHandler (fst command) (snd command, update)
         else pure []
    _ -> pure []

commandProcess :: Update -> Config -> IO ()
commandProcess update config = do
  msgs <- getMsgs2Send update
  traverse_ (`sendTextMsg` config) msgs

checkPluginRequirements :: IO ()
checkPluginRequirements = do
  let rqmt = mconcat [ timerRqmt
                     , sfRqmt
                     , noteRqmt
                     ]
  de <- doesDirectoryExist "wldata"
  _ <- if de then pure () else createDirectory "wldata"
  traverse_ (\fileName -> do
    fe <- doesFileExist fileName
    if fe then pure () else writeFile fileName "") rqmt

type Microsecond = Int
oneMin :: Microsecond
oneMin = 60000000

-- | An automatic operation which checks plugin events every 1 minute.
checkPluginEventsIn1Min :: Config -> IO ()
checkPluginEventsIn1Min config = forever $ do
  msgs <- sequence [checkTimer]
  traverse_ (`sendTextMsg` config) $ mconcat msgs
  threadDelay oneMin

checkPluginEventsIn1Day :: Config -> IO ()
checkPluginEventsIn1Day config = forever $ do
  msgs <- sequence [checkNewOfSolidot]
  traverse_ (`sendTextMsg` config) $ mconcat msgs
  threadDelay (oneMin*60*24)

-- | "Delay" is in the unit of minutes.
sendMsgWithDelay :: Int -> Config -> [SendMsg] -> IO ()
sendMsgWithDelay delay config =
  traverse_ (\msg -> sendTextMsg msg config >> threadDelay (delay*oneMin))

getCommandHelps :: (Text.Text, Update) -> IO [SendMsg]
getCommandHelps (_, update) = do
  let helps = (mconcat.intersperse "\n")
                 [ baikeHelps
                 , noteHelps
                 , timerHelps
                 , diceHelps
                 , solidotHelps
                 , snaoHelps
                 , a2dHelps
                 ]
  pure [makeReqFromUpdate update helps]
