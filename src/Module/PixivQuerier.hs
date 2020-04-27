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
import           Core.Type.EitherT

import qualified Data.Text                    as Text

packPixivImgUrl :: Text.Text -> String
packPixivImgUrl = Text.unpack.("https://pixiv.cat/"<>) . (<>".jpg")

processPixivQuery :: (Text.Text, Update) -> IO [SendMsg]
processPixivQuery (cmdBody, update) = do
  x' <- runMEitherT $ do
         rsp <- liftEither getErrHint $ try (get $ packPixivImgUrl cmdBody)
         let imgCachePath = "images/Pixiv-" <> Text.unpack cmdBody <> ".jpg"
         exist <- lift (doesFileExist imgCachePath)
         if not exist
            then lift $ BL.writeFile imgCachePath $ rsp ^. responseBody
            else lift $ pure ()
         pure $ Text.pack $ drop 7 imgCachePath
  let x = getTextT x'

  -- Check if error occur
  if snd (Text.breakOn "jpg" x) == ""
     then pure [makeReqFromUpdate update x]
     else pure [makeReqFromUpdate'' update (Text.unpack x) ""]

getErrHint :: SomeException -> Text.Text
getErrHint err
  | snd (Text.breakOn "無法取得" $ Text.pack $ show err) /= "" = "该PID不存在。"
  | snd (Text.breakOn "需要指定" $ Text.pack $ show err) /= "" = "该PID中含多张作品，请指定序号。"
  | otherwise = "未知错误。"
