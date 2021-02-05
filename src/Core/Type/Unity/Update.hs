module Core.Type.Unity.Update where

import           Data.List
import           Database.SQLite.Simple

data Update = Update
  { platform           :: Int
  , user_id            :: Integer
  , chat_id            :: Integer
  , message_text       :: Maybe String
  , message_image_urls :: [String]
  , message_type       :: Int
  , message_id         :: Integer
  , reply_id           :: Maybe Integer
  }
  deriving (Eq, Show)
instance FromRow Update where
  fromRow =
    Update
      <$> field
      <*> field
      <*> field
      <*> field
      <*> (splitOn ',' <$> field)
      <*> field
      <*> field
      <*> field
   where
    splitOn _ [] = []
    splitOn c str =
      let x = takeWhile (/= c) str in x : splitOn c (drop (length x + 1) str)

instance ToRow Update where
  toRow Update { platform = p, user_id = uid, chat_id = cid, message_text = mt, message_image_urls = miu, message_type = mtp, message_id = mid, reply_id = rid }
    = toRow (p, uid, cid, mt, (mconcat . intersperse "," $ miu), mtp, mid, rid)
