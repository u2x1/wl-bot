module Core.Data.Unity where

import Core.Type.Unity.Update    as UN
import Core.Type.Telegram.Update as TG
import Core.Type.CoolQ.Update    as CQ
import Core.Data.CoolQ           as CQ

makeUpdateFromTG :: TG.Update -> Maybe UN.Update
makeUpdateFromTG tgUpdate = UN.Update <$> userId <*> msgId  <*> chatId <*> Nothing <*> usrNick <*> msgTxt
  where msg     = TG.message tgUpdate
        userId  = TG.user_id         . TG.from <$> msg
        msgId   = TG.message_id                <$> msg
        chatId  = TG.chat_id         . TG.chat <$> msg
        usrNick = TG.user_first_name . TG.from <$> msg
        msgTxt  = TG.text                      =<< msg

makeUpdateFromCQ :: CQ.Update -> Maybe UN.Update
makeUpdateFromCQ cqUpdate = UN.Update <$> userId <*> msgId <*> chatId <*> Just groupId <*> usrNick <*> pure msgTxt
  where userId  = CQ.user_id                                  cqUpdate
        groupId = CQ.group_id                                 cqUpdate
        msgId   = CQ.message_id                               cqUpdate
        chatId  = CQ.group_id                                 cqUpdate
        usrNick = CQ.nickname <$> sender                      cqUpdate
        msgTxt  = CQ.getText . CQ.message                 $   cqUpdate
