{-# LANGUAGE OverloadedStrings #-}
module Module.PixivQuerier where

import           Core.Type.Unity.Update       as UU
import           Core.Data.Unity
import           Core.Type.Unity.Request      as UR
import           Network.Wreq
import           Control.Lens
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
       r <- get $ packPixivImgUrl cmdBody
       let imgCachePath = "pixivCache.jpg"
       BL.writeFile ("images/"<>imgCachePath) $ r ^. responseBody
       pure [makeReqFromUpdate'' update (Just $ Text.pack imgCachePath) Nothing]
     else pure [makeReqFromUpdate update "无效PID。"]
