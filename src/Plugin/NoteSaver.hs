{-# LANGUAGE OverloadedStrings #-}
module Plugin.NoteSaver where

import           Core.Type.Unity.Update       as UU
import           Core.Type.Unity.Request      as UR

import Utils.Logging

import Data.Text                 as Text
import Data.Text.IO              as Text

searchBetween :: Text -> Text -> Text -> Maybe Text
searchBetween left right content =
  case breakOnEnd left content of
    ("",_) -> Nothing
    (_, fstRound) -> Just $ fst (breakOn right fstRound)

saveNote :: Text -> Text -> IO ()
saveNote key value = Text.appendFile "notes.txt" ("`{"<>key<>">`"<>value<>"!^\n")

queryNote :: Text -> IO (Maybe Text)
queryNote key = do
  notes <- Text.readFile "notes.txt"
  pure $ searchBetween ("`{"<>key<>">`") "!^" notes

processNoteOp :: Update -> IO [SendMsg]
processNoteOp update =
  let msgTxt = message_text update in
    case Text.take 4 msgTxt of

      -- Saving note.
      "/sn " ->
        let content = Text.dropWhile (==' ') (Text.drop 4 msgTxt) in
        if Text.dropWhile (/=' ') content /= ""
          then do
            let key = Text.takeWhile (/=' ') content
                value = Text.dropWhile (/=' ') content
            _ <- saveNote key value
            logWT Info $
              "Note: ["<>Text.unpack key<>"]"<>Text.unpack value<>"saved from"<>show (user_id update)<>"."
            pure [SendMsg "Note saved." (UU.chat_id update) (UU.message_type update) (UU.platform update)]
          else pure []

      -- Query notes.
      "/qn " ->
        if Text.replace " " "" msgTxt /= "/qn"
           then do
             note <- queryNote (Text.replace " " "" $ Text.drop 4 msgTxt)
             case note of
               Just n -> pure [SendMsg n (UU.chat_id update) (UU.message_type update) (UU.platform update)]
               _      -> pure [SendMsg "No note found." (UU.chat_id update) (UU.message_type update) (UU.platform update)]
            else pure []

      _ -> pure []
