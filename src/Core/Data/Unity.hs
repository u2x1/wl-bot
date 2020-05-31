{-# LANGUAGE OverloadedStrings #-}
module Core.Data.Unity where

import           Core.Type.Unity.Update    as UU
import           Core.Type.Mirai.Update    as MU
import           Core.Type.Telegram.Update as TU

import           Core.Type.Unity.Request   as UR (SendMsg(SendMsg))
import           Core.Type.Universal
import           Core.Data.MsgLog
import           Core.Data.Mirai                 (getText, getImgUrls)
import           Data.Maybe                      (catMaybes)
import           Data.Text                       (Text)

-- | Create payload with local image paths and text.
makeReqFromUpdate'' :: UU.Update -> String -> Text -> UR.SendMsg
makeReqFromUpdate'' update imgpath txt =
  fromUpdate update Nothing (Just imgpath) (Just txt)

-- | Create payload with image urls and text.
makeReqFromUpdate' :: UU.Update -> Text -> Text -> UR.SendMsg
makeReqFromUpdate' update imgurl txt =
  fromUpdate update (Just imgurl) Nothing (Just txt)

-- | Create payload with only text.
makeReqFromUpdate :: UU.Update -> Text -> UR.SendMsg
makeReqFromUpdate update txt =
  fromUpdate update Nothing Nothing (Just txt)

fromUpdate :: UU.Update -> Maybe Text -> Maybe String -> Maybe Text -> SendMsg
fromUpdate update = UR.SendMsg (UU.chat_id update) (UU.user_id update)(UU.message_type update) (UU.platform update) (Just $ UU.message_id update)

-- | Transform Telegram updates to Unity update.
makeUpdateFromTG :: TU.Update -> Maybe UU.Update
makeUpdateFromTG tgUpdate = UU.Update <$> Just Telegram <*> userId <*> chatId <*> pure msgTxt <*> Just Nothing <*> msgType <*> msgId <*> msgRplId
  where msg      = TU.message tgUpdate
        userId   = TU.user_id         . TU.tgm_from <$> msg
        msgId    = TU.tgm_message_id               <$> msg
        chatId   = TU.chat_id         . TU.tgm_chat <$> msg
        msgTxt   = TU.tgm_text                     =<< msg
        msgRplId = TU.tgm_reply_id                 <$> msg
        msgType  = case TU.chat_type.TU.tgm_chat <$> msg of
                    Just "private"      -> Just Private
                    Just "group"        -> Just Group
                    Just "supergroup"   -> Just Group
                    _         -> Nothing

-- | Transform Mirai updates to Unity update.
makeUpdateFromMR :: MU.Update -> Maybe UU.Update
makeUpdateFromMR cqUpdate = UU.Update <$> Just QQ <*> userId <*> chatId <*> pure msgTxt <*> pure msgImg <*> msgType <*> msgId <*> pure msgRplId
  where userId   = Just . MU.mrs_id  . MU.mirai_sender                $ cqUpdate
        msgId    = Just . MU.mirai_message_id                        $ cqUpdate
        msgTxt   = getText . MU.mirai_message_chain                $ cqUpdate
        msgRplId = MU.mirai_reply_id                                   cqUpdate
        msgImg   = getImgUrls    (MU.mirai_message_chain cqUpdate)
        chatId   = case MU.mrs_group_id $ MU.mirai_sender  cqUpdate of
                     Nothing -> userId
                     grpid   -> grpid
        msgType  = case MU.mirai_type cqUpdate of
                     "FriendMessage" -> Just Private
                     "TempMessage"   -> Just Temp
                     "GroupMessage"  -> Just Group
                     _        -> Nothing

-- | Transform Mirai updates to Unity update, but fetch the origin reply message from local log.
makeUpdateFromMR' :: MU.Update -> IO (Maybe UU.Update)
makeUpdateFromMR' cqUpdate = do
  qtImg <- case UU.reply_id =<< update of
             Just rId -> do
               m <- fetchMsg rId
               pure $ message_image_urls =<< m
             Nothing -> pure Nothing
  return $ UU.Update <$> Just QQ <*> userId <*> chatId <*> msgTxt <*> ((qtImg <>) <$> msgImg) <*> msgType <*> msgId <*> msgRplId
  where update   = makeUpdateFromMR cqUpdate
        userId   = UU.user_id            <$> update
        msgId    = UU.message_id         <$> update
        msgTxt   = UU.message_text       <$> update
        msgRplId = UU.reply_id           <$> update
        msgImg   = UU.message_image_urls <$> update
        chatId   = UU.chat_id            <$> update
        msgType  = UU.message_type       <$> update
