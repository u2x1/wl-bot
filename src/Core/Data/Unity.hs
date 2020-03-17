module Core.Data.Unity where

import Core.Type.Unity.Update    as UN
import Core.Type.Telegram.Update as TG
import Core.Type.CoolQ.Update    as CQ
import Core.Data.CoolQ           as CQ
import Core.Type.Universal

makeUpdateFromTG :: TG.Update -> Maybe UN.Update
makeUpdateFromTG tgUpdate = UN.Update <$> Just Telegram <*> userId <*> userNick  <*> chatId <*> msgTxt <*> msgType <*> msgId
  where msg     = TG.message tgUpdate
        userId  = TG.user_id         . TG.from <$> msg
        msgId   = TG.message_id                <$> msg
        chatId  = TG.chat_id         . TG.chat <$> msg
        userNick = TG.user_first_name . TG.from <$> msg
        msgTxt  = TG.text                      =<< msg
        msgType = case TG.chat_type.TG.chat <$> msg of
                    Just "private"      -> Just Private
                    Just "group"        -> Just Group
                    Just "supergroup"   -> Just Group
                    _         -> Nothing

makeUpdateFromCQ :: CQ.Update -> Maybe UN.Update
makeUpdateFromCQ cqUpdate = UN.Update <$> Just QQ <*> userId <*> userNick <*> chatId <*> pure msgTxt <*> msgType <*> msgId
  where userId   = CQ.user_id                     cqUpdate
        msgId    = CQ.message_id                  cqUpdate
        chatId   = CQ.group_id                    cqUpdate
        userNick = CQ.nickname <$> CQ.sender      cqUpdate
        msgTxt   = CQ.getText   .  CQ.message   $ cqUpdate
        msgType  = case CQ.message_type cqUpdate of
                     "friend" -> Just Private
                     "group"  -> Just Group
                     "discuss"-> Just Group
                     _        -> Nothing
