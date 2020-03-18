module Core.Data.Unity where

import           Core.Type.Unity.Update    as UN
import           Core.Type.Unity.Request   as UR
import           Core.Type.Telegram.Update as T
import           Core.Type.CoolQ.Update    as Q
import           Core.Data.CoolQ           as Q
import           Core.Type.Universal
import qualified Data.Text                 as Text

makeReqFromUpdate :: UN.Update -> Text.Text -> UR.SendMsg
makeReqFromUpdate update txt =
  UR.SendMsg txt (UN.chat_id update) (UN.message_type update) (UN.platform update)

makeUpdateFromTG :: T.Update -> Maybe UN.Update
makeUpdateFromTG tgUpdate = UN.Update <$> Just Telegram <*> userId <*> userNick <*> chatId <*> msgTxt <*> msgType <*> msgId
  where msg      = T.message tgUpdate
        userId   = T.user_id         . T.from <$> msg
        msgId    = T.message_id               <$> msg
        chatId   = T.chat_id         . T.chat <$> msg
        userNick = T.user_first_name . T.from <$> msg
        msgTxt   = T.text                      =<< msg
        msgType  = case T.chat_type.T.chat <$> msg of
                    Just "private"      -> Just Private
                    Just "group"        -> Just Group
                    Just "supergroup"   -> Just Group
                    _         -> Nothing

makeUpdateFromCQ :: Q.Update -> Maybe UN.Update
makeUpdateFromCQ cqUpdate = UN.Update <$> Just QQ <*> userId <*> userNick <*> chatId <*> pure msgTxt <*> msgType <*> msgId
  where userId   = Q.user_id                    cqUpdate
        msgId    = Q.message_id                 cqUpdate
        chatId   = Q.group_id                   cqUpdate
        userNick = Q.nickname <$> Q.sender      cqUpdate
        msgTxt   = Q.getText   .  Q.message   $ cqUpdate
        msgType  = case Q.message_type cqUpdate of
                     "friend" -> Just Private
                     "group"  -> Just Group
                     "discuss"-> Just Group
                     _        -> Nothing
