{-# LANGUAGE OverloadedStrings #-}
module Core.Data.Unity where

import           Core.Type.Mirai.Update        as MU
import           Core.Type.Telegram.Update     as TU
import           Core.Type.Unity.Update        as UU

import           Core.Data.Mirai                ( getImgUrls
                                                , getText
                                                )
import           Core.Data.MsgLog               ( fetchMsg )
import           Core.Type.Unity.Request       as UR
                                                ( SendMsg(SendMsg) )
import           Core.Type.Universal            ( Platform(..)
                                                , TargetType(..)
                                                )
import           Data.Text                      ( Text )

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
makeReqFromUpdate update txt = fromUpdate update Nothing Nothing (Just txt)

fromUpdate :: UU.Update -> Maybe Text -> Maybe String -> Maybe Text -> SendMsg
fromUpdate update = UR.SendMsg (show $ UU.chat_id update)
                               (show $ UU.user_id update)
                               (getMsgTp $ UU.message_type update)
                               (getPlat $ UU.platform update)
                               (Just $ show $ UU.message_id update)

getPlat :: (Eq a, Num a) => a -> Platform
getPlat 0 = Telegram
getPlat 1 = QQ
getPlat _ = QQ

getMsgTp :: (Eq a, Num a) => a -> TargetType
getMsgTp 0 = Private
getMsgTp 1 = Group
getMsgTp 2 = Temp
getMsgTp _ = Temp

-- | Transform Telegram updates to Unity update.
makeUpdateFromTG :: TU.Update -> Maybe UU.Update
makeUpdateFromTG tgUpdate =
  UU.Update
    <$> Just 0
    <*> userId
    <*> chatId
    <*> pure msgTxt
    <*> Just []
    <*> msgType
    <*> msgId
    <*> msgRplId
 where
  msg      = TU.message tgUpdate
  userId   = TU.user_id . TU.tgm_from <$> msg
  msgId    = TU.tgm_message_id <$> msg
  chatId   = TU.chat_id . TU.tgm_chat <$> msg
  msgTxt   = TU.tgm_text =<< msg
  msgRplId = TU.tgm_reply_id <$> msg
  msgType  = case TU.chat_type . TU.tgm_chat <$> msg of
    Just "private"    -> Just 1
    Just "group"      -> Just 2
    Just "supergroup" -> Just 2
    _                 -> Nothing

-- | Transform Mirai updates to Unity update.
makeUpdateFromMR :: MU.Update -> Maybe UU.Update
makeUpdateFromMR cqUpdate =
  UU.Update
    <$> Just 1
    <*> userId
    <*> chatId
    <*> pure msgTxt
    <*> pure msgImg
    <*> msgType
    <*> msgId
    <*> pure msgRplId
 where
  userId   = Just . MU.mrs_id . MU.mirai_sender $ cqUpdate
  msgId    = Just . MU.mirai_message_id $ cqUpdate
  msgTxt   = getText . MU.mirai_message_chain $ cqUpdate
  msgRplId = MU.mirai_reply_id cqUpdate
  msgImg   = getImgUrls (MU.mirai_message_chain cqUpdate)
  chatId   = case MU.mrs_group_id $ MU.mirai_sender cqUpdate of
    Nothing -> userId
    grpid   -> grpid
  msgType = case MU.mirai_type cqUpdate of
    "FriendMessage" -> Just 0
    "TempMessage"   -> Just 2
    "GroupMessage"  -> Just 1
    _               -> Nothing

-- | Transform Mirai updates to Unity update, but fetch the origin reply message from local log.
makeUpdateFromMR' :: MU.Update -> IO (Maybe UU.Update)
makeUpdateFromMR' cqUpdate = do
  qtImg <- case UU.reply_id =<< update of
    Just rId -> do
      m <- fetchMsg rId
      case m of
        Just x -> pure $ message_image_urls x
        _      -> pure []
    Nothing -> pure []
  return
    $   UU.Update
    <$> Just 1
    <*> userId
    <*> chatId
    <*> msgTxt
    <*> ((qtImg <>) <$> msgImg)
    <*> msgType
    <*> msgId
    <*> msgRplId
 where
  update   = makeUpdateFromMR cqUpdate
  userId   = UU.user_id <$> update
  msgId    = UU.message_id <$> update
  msgTxt   = UU.message_text <$> update
  msgRplId = UU.reply_id <$> update
  msgImg   = UU.message_image_urls <$> update
  chatId   = UU.chat_id <$> update
  msgType  = UU.message_type <$> update
