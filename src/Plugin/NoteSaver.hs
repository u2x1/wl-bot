{-# LANGUAGE OverloadedStrings #-}
module Plugin.NoteSaver where

import           Core.Web.CoolQ               as CQ
import           Core.Type.CoolQ.Update       as CQ
import           Core.Data.CoolQ              as CQ

import Utils.Config
import Utils.Logging

import Control.Concurrent
import Data.Text                 as Text
import Data.Text.IO              as Text

searchBetween :: Text -> Text -> Text -> Text
searchBetween left right content =
  let fstround = snd $ breakOnEnd left content in
      fst (breakOn right fstround)

saveNote :: Text -> Text -> IO ()
saveNote key value = Text.appendFile "notes.txt" ("`{"<>key<>">`"<>value<>"!^\n")

queryNote :: Text -> IO (Maybe Text)
queryNote key = do
  notes <- Text.readFile "notes.txt"
  case searchBetween ("`{"<>key<>">`") "!^" notes of
    "" -> pure Nothing
    value  -> pure $ Just value

processNoteOp :: Config -> CQ.Update -> IO (Maybe ThreadId)
processNoteOp config cqUpdate =
  let msgTxt = getText (CQ.message cqUpdate) in
    case Text.take 4 msgTxt of
      "/sn " ->
        if Prelude.length (splitOn " " msgTxt) == 3
          then do
            let inputs = splitOn " " msgTxt
            _ <- saveNote (inputs!!1) (inputs!!2)
            logWT Info (Text.unpack $ "Note: [" <> (inputs!!1) <> "]" <> (inputs!!2) <> "saved.")
            Just <$> CQ.sendBackTextMsg "Note saved." cqUpdate config
          else pure Nothing
      "/qn " ->
        if Text.replace " " "" msgTxt /= "/qn"
           then do
             note <- queryNote (Text.replace " " "" $ Text.drop 4 msgTxt)
             case note of
               Just n -> Just <$> CQ.sendBackTextMsg n cqUpdate config
               _      -> Just <$> CQ.sendBackTextMsg "No note found." cqUpdate config
            else pure Nothing
      _ -> pure Nothing

