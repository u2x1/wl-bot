{-# LANGUAGE OverloadedStrings #-}
module Core.Data.PluginConsole where

import qualified Data.Text as Text
import           Core.Type.Unity.Update
import           Core.Type.Unity.Request

import           Plugin.BaikeQuerier
import           Plugin.NoteSaver

commandProcess :: Update -> IO [SendMsg]
commandProcess update =
      if Text.head msgTxt /= '/'
         then pure []
         else do
           let command = Text.breakOn " " msgTxt
           case fst command of
             "/qr" -> processQuery (snd command, update)
             "/sn" -> saveNote (snd command, update)
             "/qn" -> queryNote (snd command, update)
  where msgTxt = message_text update
