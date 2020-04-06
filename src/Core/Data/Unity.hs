{-# LANGUAGE OverloadedStrings #-}
module Core.Data.Unity where

import           Core.Type.Unity.Update    as UN
import           Core.Type.Unity.Request   as UR
import           Core.Type.Telegram.Update as T
import           Core.Type.Mirai.Update    as Q
import           Core.Data.Mirai           as Q
import           Core.Type.Universal
import qualified Data.Text                 as Text

makeReqFromUpdate' :: UN.Update -> Maybe [Text.Text] -> Maybe Text.Text -> UR.SendMsg
makeReqFromUpdate' update =
  UR.SendMsg (UN.chat_id update) (UN.message_type update) (UN.platform update) (Just $ UN.message_id update)

makeReqFromUpdate :: UN.Update -> Text.Text -> UR.SendMsg
makeReqFromUpdate update txt =
  UR.SendMsg (UN.chat_id update) (UN.message_type update) (UN.platform update) (Just $ UN.message_id update) Nothing (Just txt)

makeUpdateFromTG :: T.Update -> Maybe UN.Update
makeUpdateFromTG tgUpdate = UN.Update <$> Just Telegram <*> userId <*> chatId <*> pure msgTxt <*> Just Nothing <*> msgType <*> msgId
  where msg      = T.message tgUpdate
        userId   = T.user_id         . T.from <$> msg
        msgId    = T.message_id               <$> msg
        chatId   = T.chat_id         . T.chat <$> msg
        msgTxt   = T.text                      =<< msg
        msgType  = case T.chat_type.T.chat <$> msg of
                    Just "private"      -> Just Private
                    Just "group"        -> Just Group
                    Just "supergroup"   -> Just Group
                    _         -> Nothing

makeUpdateFromMR :: Q.Update -> Maybe UN.Update
makeUpdateFromMR cqUpdate = UN.Update <$> Just QQ <*> userId <*> chatId <*> pure msgTxt <*> pure msgImage <*> msgType <*> msgId
  where userId   = Just $ Q.mrs_id .  Q.mirai_sender   $ cqUpdate
        msgId    = Just $ Q.mirai_message_id           $ cqUpdate
        msgTxt   = Q.getText.Q.mirai_message_chain $ cqUpdate
        msgImage = Q.getImgUrls.Q.mirai_message_chain$ cqUpdate
        chatId   = case Q.mrs_group_id . Q.mirai_sender$cqUpdate of
                     Nothing -> userId
                     grpid   -> grpid
        msgType  = case Q.mirai_type cqUpdate of
                     "FriendMessage" -> Just Private
                     "GroupMessage"  -> Just Group
                     _        -> Nothing
