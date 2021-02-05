{-# LANGUAGE OverloadedStrings #-}
module Module.NoteSaver where

import           Core.Type.Unity.Request       as UR
                                                ( SendMsg )
import           Core.Type.Unity.Update        as UU
                                                ( Update(user_id) )

import           Core.Data.Unity                ( makeReqFromUpdate )
import           Utils.Logging                  ( LogTag(Info)
                                                , logWT
                                                )
import           Utils.Misc                     ( searchBetweenText )

import           Data.Text                     as Text
                                                ( Text
                                                , dropWhile
                                                , null
                                                , strip
                                                , takeWhile
                                                , unpack
                                                )
import           Data.Text.IO                  as Text
                                                ( appendFile
                                                , readFile
                                                )

writeNoteFile :: Text -> Text -> IO ()
writeNoteFile key value = Text.appendFile
  (Prelude.head noteRqmt)
  ("`{" <> key <> ">`" <> value <> "!^\n")

readNoteFile :: Text -> IO (Maybe Text)
readNoteFile key =
  Text.readFile (Prelude.head noteRqmt)
    >>= (pure . searchBetweenText ("`{" <> key <> ">`") "!^")

queryNote :: (Text, Update) -> IO [SendMsg]
queryNote (cmdBody, update) = if not $ Text.null content
  then do
    note <- readNoteFile content
    case note of
      Just n -> pure [makeReqFromUpdate update n]
      _      -> pure [makeReqFromUpdate update "未找到。"]
  else pure []
  where content = Text.strip cmdBody

saveNote :: (Text, Update) -> IO [SendMsg]
saveNote (cmdBody, update) = if content /= ""
  then do
    let key   = Text.takeWhile (/= ' ') content
        value = Text.dropWhile (/= ' ') content
    _ <- writeNoteFile key value
    logWT Info
      $  "Note: ["
      <> Text.unpack key
      <> "]"
      <> Text.unpack value
      <> "saved from"
      <> show (user_id update)
      <> "."
    pure [makeReqFromUpdate update "已保存。"]
  else pure []
  where content = Text.strip cmdBody

noteRqmt :: [String]
noteRqmt = fmap ("wldata/" <>) ["NT-notes.txt"]
