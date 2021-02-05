{-# LANGUAGE OverloadedStrings #-}
module Core.Module.Console where

import           Control.Concurrent             ( threadDelay )
import           Core.Data.MsgLog               ( initMsgLogDB
                                                , logMsg
                                                )
import           Core.Data.Unity                ( makeReqFromUpdate )
import           Core.Type.Unity.Request        ( SendMsg )
import           Core.Type.Unity.Update         ( Update
                                                , message_text
                                                )
import           Core.Web.Unity                 ( sendMsg )
import           Data.Foldable                  ( traverse_ )
import           Data.Text                     as T
                                                ( Text
                                                , breakOn
                                                , pack
                                                , strip
                                                , tail
                                                )
-- import           Module.YandeFetcher
import           Module.Ascii2dSearcher
-- import         Utils.Logging
import           Module.BaikeQuerier
import           Module.DiceHelper
import           Module.GetImgUrl
import           Module.JavDBSearcher
-- import           Module.HostlocFeed
import           Module.NHentaiQuerier
import           Module.NoteSaver
import           Module.PixivQuerier
import           Module.SauceNAOSearcher
-- import           Module.BilibiliHelper
import           Module.SolidotFetcher
import           Module.TorrentSearcher
import           Module.WAITSearcher
import           System.Directory               ( createDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
                                                )
import           Utils.Config                   ( Config )
import qualified Utils.Misc                    as Misc
                                                ( unlines )

import           Control.Exception
import           Control.Monad

commandProcess :: Update -> Config -> IO ()
commandProcess update config = do
  _    <- logMsg update
  msgs <- getMsgs2Send update
  traverse_ (`sendMsg` config) msgs

getMsgs2Send :: Update -> IO [SendMsg]
getMsgs2Send update = case dropWhile (== ' ') <$> message_text update of
  Just msgTxt ->
    if not (null msgTxt)
         && (head msgTxt == '/' || head msgTxt == '.' || head msgTxt == '。')
      then do
        let command = T.breakOn " " $ T.pack msgTxt
        case getHandler (fst command) of
          Just handler -> do
            msgs <- handler (T.strip $ snd command, update)
            if null msgs
              then case (getCmdHelp (fst command)) of
                Just help -> pure $ [makeReqFromUpdate update help]
                _         -> pure []
              else pure msgs
          _ -> pure []
      else pure []
  _ -> pure []

getHandler :: Text -> Maybe ((Text, Update) -> IO [SendMsg])
getHandler cmdHeader = fst <$> lookup (T.tail cmdHeader) commands

getCmdHelp :: Text -> Maybe Text
getCmdHelp cmdHeader =
  (cmdHeader <>) . snd . snd <$> lookup (T.tail cmdHeader) commands

checkModuleRequirements :: IO ()
checkModuleRequirements = do
  let rqmt = mconcat
        [ sfRqmt
                    --  , hlRqmt
        , noteRqmt
        ]
      drctRqmt = mconcat [trtDrctRqmt]
  de <- doesDirectoryExist "wldata"
  _  <- if de then pure () else createDirectory "wldata"
  traverse_
    (\fileName -> do
      fe <- doesFileExist fileName
      if fe then pure () else writeFile fileName ""
    )
    rqmt
  traverse_
    (\drctName -> do
      fe <- doesDirectoryExist drctName
      if fe then pure () else createDirectory drctName
    )
    drctRqmt

  initMsgLogDB
  -- initHostlocDB

type Microsecond = Int
oneMin :: Microsecond
oneMin = 60000000

checkModuleEventsIn1Day :: Config -> IO ()
checkModuleEventsIn1Day config =
  forever
    . void
    $ ((try $ do
         msgs <- sequence [checkNewOfSolidot]
         traverse_ (`sendMsg` config) $ mconcat msgs
         threadDelay (oneMin * 60 * 6)
       ) :: IO (Either SomeException ())
      )

-- checkModuleEventsIn5Mins :: Config -> IO ()
-- checkModuleEventsIn5Mins config = forever . void $ ((try $ do
--   msgs <- sequence [checkNewOfHostloc]
--   traverse_ (`sendMsg` config) $ mconcat msgs
--   threadDelay (oneMin*5)) :: IO (Either SomeException ()))

sendMsgWithDelay :: Int -> Config -> [SendMsg] -> IO ()
sendMsgWithDelay delay config =
  traverse_ (\x -> sendMsg x config >> threadDelay delay)

-- Module: Help --

getCommandHelps :: (Text, Update) -> IO [SendMsg]
getCommandHelps (cmdBody, update) = if cmdBody /= ""
  then case lookup cmdBody commands of
    Just (_, (_, h)) -> pure [makeReqFromUpdate update ("/" <> cmdBody <> h)]
    Nothing          -> pure [makeReqFromUpdate update "未找到指令。"]
  else pure
    [ makeReqFromUpdate update
      $  "! 使用/help <command>查看每条指令的详细信息\n"
      <> Misc.unlines
           (fmap (\(cmd, (_, (c, _))) -> "/" <> cmd <> ": " <> c) commands)
    ]

commands :: [(Text, ((Text, Update) -> IO [SendMsg], (Text, Text)))]
commands =
  [ ("bk", (processBaiduQuery, ("百科摘要", " <entry>: 从baike.baidu.com抓取摘要")))
  , ("svnote"  , (saveNote, ("笔记", " <key> <value>: 由机器人上传一条信息到服务器保存")))
  , ("note"    , (queryNote, ("笔记", " <key>: 查找对应的信息")))
  , ("subsd"   , (addSubscriber, ("新闻", " : 订阅solidot.org")))
  , ("cxlsubsd", (rmSubscribe, ("新闻", " : 取消订阅solidot.org")))
  , ("dc"      , (processDiceRolling, ("骰子", " <dice>: 生成随机骰子(如d6,2d10)")))
  , ("sp"      , (processSnaoQuery, ("搜图", " {pic}: 使用图片从saucenao.com搜图")))
  , ("spu", (processAscii2dSearch, ("搜图", " {pic}: 使用图片获得Ascii2d与Yandex搜图链接")))
  , ("nht", (processNHentaiQuery, ("搜本子", " <name>: 使用本子名从NHentai.net查询本子")))
  , ("fh"      , (processJavDBQuery, ("搜番号", " <id>: 从JavDB查询番号")))
  , ("am", (processWAITQuery, ("搜番", " {pic}: 使用图片从trace.moe(WAIT)查询番剧名")))
  , ("pid", (processPixivQuery, ("Pixiv ID", " <pid>: 使用PID从pixiv.cat取得图片")))
  , ( "trt"
    , (processTrtQurey, ("搜种子", " <keyword> <index>: 从torrentkitty.tv搜索种子"))
    )
  , ("qurl", (processImgUrlFetch, ("图片链接", " {pic}: 获取QQ图片的在线链接")))
--  , ("bili"    , (processBiliQuery      , ("哔哩哔哩", " ID: 使用AV号或BV号从哔哩哔哩获取下载链接")))
  , ("help", (getCommandHelps, ("帮助", " <command>: 查看帮助")))
  ]
