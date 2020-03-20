{-# LANGUAGE OverloadedStrings #-}
module Core.Plugin.Console where

import qualified Data.Text as Text
import           Data.List
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

getHandler :: Text.Text -> ((Text.Text, Update) -> IO [SendMsg])
getHandler cmdHeader =
  case Text.toLower cmdHeader of
    "/bkqr" -> processQuery

    "/svnote" -> saveNote
    "/note" -> queryNote

    "/timer" -> addTimer
    "/cxltimer" -> cancelTimer
    "/pd" -> setPomodoro

    "/dc" -> processDiceRolling

    "/help" -> getCommandHelps
    _     -> pure $ pure []

getMsgs2Send :: Update -> IO [SendMsg]
getMsgs2Send update =
  if Text.head msgTxt /= '/'
     then pure []
     else
       let command = Text.breakOn " " msgTxt in
       getHandler (fst command) (snd command, update)
  where msgTxt = message_text update

commandProcess :: Update -> Config -> IO ()
commandProcess update config = do
  msgs <- getMsgs2Send update
  void $ traverse (`sendTextMsg` config) msgs

-- This is an automatic operation which checks plugin events every 1 minute.
checkPluginEvents :: Config -> IO ()
checkPluginEvents config = forever $ do
  msgs <- checkTimer
  void $ traverse (`sendTextMsg` config) msgs
  threadDelay 60000000


getCommandHelps :: (Text.Text, Update) -> IO [SendMsg]
getCommandHelps (_, update) = do
  let helps = (mconcat.intersperse "\n\n")
                 [ baikeHelps
                 , noteHelps
                 , timerHelps
                 , diceHelps
                 ]
  pure [makeReqFromUpdate update helps]
