{-# LANGUAGE OverloadedStrings #-}
module Core.Data.MsgLog where

import           Core.Type.Unity.Update
import           Database.SQLite.Simple

initMsgLogDB :: IO ()
initMsgLogDB =
  withConnection "wldata.db" (\conn -> execute_ conn createMsgLogDB)

createMsgLogDB :: Query
createMsgLogDB = "CREATE TABLE IF NOT EXISTS msgs (platform INTEGER, \
                                                   \user_id INTEGER, \
                                                   \chat_id INTEGER, \
                                                   \message_text TEXT, \
                                                   \message_image_urls TEXT, \
                                                   \message_type INTEGER, \
                                                   \message_id INTEGER, \
                                                   \reply_id INTEGER)"


-- | Fetch a logged message from local file "wlMsg.log".
fetchMsg :: Integer -> IO (Maybe Update)
fetchMsg msgId = do
  r <- withConnection "wldata.db" (\conn -> queryNamed conn "SELECT * FROM msgs WHERE message_id = :msg_id" [":msg_id" := msgId] :: IO [Update])
  if null r then pure Nothing else pure $ Just (head r)

logMsg :: Update -> IO ()
logMsg update =
  withConnection "wldata.db" (\conn -> execute conn "INSERT OR IGNORE INTO msgs VALUES (?, ?, ?, ?, ?, ?, ?, ?)" update)
