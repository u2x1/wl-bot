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

writeNoteFile :: Text -> Text -> IO ()
writeNoteFile key value = Text.appendFile "notes.txt" ("`{"<>key<>">`"<>value<>"!^\n")

readNoteFile :: Text -> IO (Maybe Text)
readNoteFile key = do
  notes <- Text.readFile "notes.txt"
  pure $ searchBetween ("`{"<>key<>">`") "!^" notes

queryNote :: (Text, Update) -> IO [SendMsg]
queryNote (cmdBody, update) =
  if not $ Text.null content
     then do
       note <- readNoteFile content
       case note of
         Just n -> pure [SendMsg n (UU.chat_id update) (UU.message_type update) (UU.platform update)]
         _      -> pure [SendMsg "No note found." (UU.chat_id update) (UU.message_type update) (UU.platform update)]
      else pure []
  where
    content = Text.dropWhile (==' ') cmdBody

saveNote :: (Text, Update) -> IO [SendMsg]
saveNote (cmdBody, update) =
  if content /= ""
    then do
      let key = Text.takeWhile (/=' ') content
          value = Text.dropWhile (/=' ') content
      _ <- writeNoteFile key value
      logWT Info $
        "Note: ["<>Text.unpack key<>"]"<>Text.unpack value<>"saved from"<>show (user_id update)<>"."
      pure [SendMsg "Note saved." (UU.chat_id update) (UU.message_type update) (UU.platform update)]
    else pure []
  where
    content = Text.dropWhile (==' ') cmdBody

noteHelps :: Text.Text
noteHelps = "==NoteSaver==\n\
            \/svnote NAME CONTENT: Save a note to cloud.\n\
            \/note NAME: Query an existing note."
