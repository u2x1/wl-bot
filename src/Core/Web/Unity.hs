{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Core.Web.Unity where

import           Control.Exception
import           Control.Lens
import           Core.Data.Mirai               as Q
import           Core.Data.Telegram            as T
import           Core.Type.Unity.Request       as U
import           Core.Web.Mirai                as Q
import           Core.Web.Telegram             as T
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Maybe
import           Network.Wreq
import           Utils.Config
import           Utils.Logging

import           Core.Type.Universal            ( Platform(QQ, Telegram)
                                                , TargetType
                                                  ( Group
                                                  , Private
                                                  , Temp
                                                  )
                                                )

import           Utils.Env

type RB = Response ByteString

sendMsg :: U.SendMsg -> Env -> IO ()
sendMsg m e = do
  s <- try (sendMsg' m e) :: IO (Either SomeException ())
  case s of
    Right _   -> return ()
    Left  err -> logErr "Sending msg" $ show err

sendMsg' :: U.SendMsg -> Env -> IO ()
sendMsg' msg env = case msg ^. target_plat of
  Telegram -> case T.transMsg msg of
    Left  p -> () <$ postTgRequest' (config ^. tg_token) "sendPhoto" p
    Right s -> () <$ postTgRequest
      (config ^. tg_token)
      (if isJust $ msg ^. imgUrl then "sendPhoto" else "sendMessage")
      (toJSON s)
  QQ -> case msg ^. target_type of
    Private -> Q.sendPrivMsg (msg ^. chat_id) (Q.transMsg msg) env
    Temp ->
      Q.sendTempMsg (msg ^. user_id) (msg ^. chat_id) (Q.transMsg msg) env
    Group ->
      Q.sendGrpMsg (msg ^. chat_id) (Q.transMsg msg) env (msg ^. reply_id)
  where config = cfg env

retry3Times :: IO a -> IO a
retry3Times action = (catch' . catch' . catch') action
  where catch' a = catch action (\e -> const a (e :: SomeException))
