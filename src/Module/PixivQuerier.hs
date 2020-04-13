{-# LANGUAGE OverloadedStrings #-}
module Module.PixivQuerier where

import           System.Directory
import           Core.Type.Unity.Update       as UU
import           Core.Data.Unity
import           Core.Type.Unity.Request      as UR
import           Network.Wreq
import           Control.Lens
import           Control.Exception
import qualified Data.ByteString.Lazy as BL

import qualified Data.Text                    as Text
import           Utils.Logging

packPixivImgUrl :: Text.Text -> String
packPixivImgUrl = Text.unpack.("https://pixiv.cat/"<>) . (<>".jpg")

processPixivQuery :: (Text.Text, Update) -> IO [SendMsg]
processPixivQuery (cmdBody, update) =
  if cmdBody /= ""
     then do
       logWT Info $
         "PID: [" <> Text.unpack cmdBody <> "] sending from " <> show (user_id update)
       r <- try $ get $ packPixivImgUrl cmdBody :: IO (Either SomeException (Response BL.ByteString))
       case r of
         Left err -> pure $
           if (snd $ Text.breakOn "無法取得" $ Text.pack $ show err) /= ""
              then [makeReqFromUpdate update "未找到指定图片。"]
              else
                if (snd $ Text.breakOn "需要指定" $ Text.pack $ show err) /= ""
                   then [makeReqFromUpdate update "该PID中含多张作品，需要指定序号。"]
                   else [makeReqFromUpdate update "未知错误。"]
         Right rsp -> do
                       let imgCachePath = "images/Pixiv-" <> Text.unpack cmdBody <> ".jpg"
                       exist <- doesFileExist imgCachePath
                       if not exist
                          then BL.writeFile (imgCachePath) $ rsp ^. responseBody
                          else pure ()
                       pure [makeReqFromUpdate'' update (Just $ drop 7 imgCachePath) Nothing]
     else pure [makeReqFromUpdate update "无效PID。"]
