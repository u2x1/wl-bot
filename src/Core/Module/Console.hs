{-# LANGUAGE OverloadedStrings #-}
module Core.Module.Console where

import qualified Data.Text as Text
import           Data.Foldable
import           System.Directory
import           Core.Type.Unity.Update
import           Core.Type.Unity.Request
import           Core.Web.Unity
import           Core.Data.Unity
import           Control.Concurrent
import           Control.Monad
import           Utils.Config
--import Utils.Logging
import qualified Utils.Misc as Misc
import           Module.BaikeQuerier
import           Module.NoteSaver
import           Module.JavDBSearcher
import           Module.DiceHelper
--import           Module.BilibiliHelper
import           Module.SolidotFetcher
import           Module.YandeFetcher
import           Module.SauceNAOSearcher
import           Module.TorrentSearcher
import           Module.PixivQuerier
import           Module.NHentaiQuerier
import           Module.WAITSearcher
import           Module.Ascii2dSearcher
import           Module.GetImgUrl

getHandler :: Text.Text -> Maybe ((Text.Text, Update) -> IO [SendMsg])
getHandler cmdHeader = fst <$> lookup (Text.tail cmdHeader) commands

getMsgs2Send :: Update -> IO [SendMsg]
getMsgs2Send update =
  case message_text update of
    Just msgTxt ->
      if head msgTxt == '/' || head msgTxt == '.' || head msgTxt == '。'
         then do
           let command = Text.breakOn " " $ Text.pack msgTxt
           case getHandler (fst command) of
             Just handler -> handler (Text.strip $ snd command, update)
             _ -> pure []
         else pure []
    _ -> pure []

commandProcess :: Update -> Config -> IO ()
commandProcess update config = do
  msgs <- getMsgs2Send update
  traverse_ (`sendTextMsg` config) msgs

checkModuleRequirements :: IO ()
checkModuleRequirements = do
  let rqmt = mconcat [ sfRqmt
                     , ydRqmt
                     , noteRqmt]
      drctRqmt = mconcat [trtDrctRqmt]
  de <- doesDirectoryExist "wldata"
  _ <- if de then pure () else createDirectory "wldata"
  traverse_ (\fileName -> do
    fe <- doesFileExist fileName
    if fe then pure () else writeFile fileName "") rqmt
  traverse_ (\drctName -> do
    fe <- doesDirectoryExist drctName
    if fe then pure () else createDirectory drctName) drctRqmt

type Microsecond = Int
oneMin :: Microsecond
oneMin = 60000000

checkModuleEventsIn1Day :: Config -> IO ()
checkModuleEventsIn1Day config = forever $ do
  msgs <- sequence [checkNewOfSolidot, checkYandePopImgs]
  traverse_ (`sendTextMsg` config) $ mconcat msgs
  threadDelay (oneMin*60*24)

sendMsgWithDelay :: Int -> Config -> [SendMsg] -> IO ()
sendMsgWithDelay delay config = traverse_ (\x -> sendTextMsg x config >> threadDelay delay)

-- Module: Help --

getCommandHelps :: (Text.Text, Update) -> IO [SendMsg]
getCommandHelps (cmdBody, update) =
  if cmdBody /= ""
     then case lookup cmdBody commands of
            Just (_, (_, h)) -> pure [makeReqFromUpdate update ("/" <> cmdBody <> h)]
            Nothing -> pure [makeReqFromUpdate update "未找到指令。"]
     else
       pure [makeReqFromUpdate update $ "使用/help COMMAND查看详细\n" <> Misc.unlines
         (fmap (\(cmd, (_,(c,_))) ->"/" <> cmd <> ": "<> c) commands)]

commands :: [(Text.Text, ((Text.Text, Update) -> IO [SendMsg], (Text.Text, Text.Text)))]
commands =
  [ ("bk"      , (processBaiduQuery     , ("百科摘要", " ENTRY: 从baike.baidu.com抓取摘要")))
  , ("svnote"  , (saveNote              , ("笔记"    , " KEY CONTENT: 由机器人上传一条信息到服务器保存")))
  , ("note"    , (queryNote             , ("笔记"    , " KEY: 查找对应的信息")))
  , ("subsd"   , (addSubscriber         , ("新闻"    , " : 订阅solidot.org")))
  , ("cxlsubsd", (rmSubscribe           , ("新闻"    , " : 取消订阅solidot.org")))
  , ("dc"      , (processDiceRolling    , ("骰子"    , " DICE: 生成随机骰子(如d6,2d10)")))
  , ("sp"      , (processSnaoQuery      , ("搜图"    , " PIC: 使用图片从saucenao.com搜图")))
  , ("asc"     , (processAscii2dSearch  , ("搜图"    , " PIC: 使用图片从ascii2d.net搜图")))
  , ("nht"     , (processNHentaiQuery   , ("搜本子"  , " NAME: 使用本子名从NHentai.net查询本子")))
  , ("fh"      , (processJavDBQuery     , ("搜番号"  , " NUMBER: 从JavDB查询番号")))
  , ("am"      , (processWAITQuery      , ("搜番"    , " PIC: 使用图片从trace.moe(WAIT)查询番剧名")))
  , ("pid"     , (processPixivQuery     , ("Pixiv ID", " PID: 使用PID从pixiv.cat取得图片")))
  , ("trt"     , (processTrtQurey       , ("搜种子"  , " KEYWORD <INDEX>: 从torrentkitty.tv搜索种子")))
  , ("qurl"    , (processImgUrlFetch    , ("图片链接", " PIC: 获取QQ图片的在线链接")))
--  , ("bili"    , (processBiliQuery      , ("哔哩哔哩", " ID: 使用AV号或BV号从哔哩哔哩获取下载链接")))
  , ("help"    , (getCommandHelps       , ("帮助"    , " COMMAND: 查看帮助")))]
