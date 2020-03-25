{-# LANGUAGE OverloadedStrings #-}
module Plugin.NoteSaver where

import           Core.Type.Unity.Update       as UU
import           Core.Type.Unity.Request      as UR

import           Core.Data.Unity
import           Utils.Logging
import           Utils.Misc                              (searchBetweenText)

import           Data.Text                    as Text
import           Data.Text.IO                 as Text

writeNoteFile :: Text -> Text -> IO ()
writeNoteFile key value = Text.appendFile (Prelude.head noteRqmt) ("`{"<>key<>">`"<>value<>"!^\n")

readNoteFile :: Text -> IO (Maybe Text)
readNoteFile key = Text.readFile (Prelude.head noteRqmt) >>= (pure.searchBetweenText ("`{"<>key<>">`") "!^")

queryNote :: (Text, Update) -> IO [SendMsg]
queryNote (cmdBody, update) =
  if not $ Text.null content
     then do
       note <- readNoteFile content
       case note of
         Just n -> pure [makeReqFromUpdate update n]
         _      -> pure [makeReqFromUpdate update "未找到指定笔记。"]
      else pure [makeReqFromUpdate update noteHelps]
  where
    content = Text.strip cmdBody

saveNote :: (Text, Update) -> IO [SendMsg]
saveNote (cmdBody, update) =
  if content /= ""
    then do
      let key = Text.takeWhile (/=' ') content
          value = Text.dropWhile (/=' ') content
      _ <- writeNoteFile key value
      logWT Info $
        "Note: ["<>Text.unpack key<>"]"<>Text.unpack value<>"saved from"<>show (user_id update)<>"."
      pure [makeReqFromUpdate update "笔记已保存。"]
    else pure [makeReqFromUpdate update noteHelps]
  where
    content = Text.strip cmdBody

noteRqmt :: [String]
noteRqmt = fmap ("wldata/" <> ) ["NT-notes.txt"]

noteHelps :: Text.Text
noteHelps = Text.unlines [ "====NoteSaver===="
                         , "/svnote NAME CONTENT: 将笔记保存到服务器"
                         , "/note NAME: 查询一条存在的笔记"]
