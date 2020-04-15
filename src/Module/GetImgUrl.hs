{-# LANGUAGE OverloadedStrings #-}
module Module.GetImgUrl where

import           Core.Type.Unity.Update
import           Core.Type.Unity.Request
import           Core.Data.Unity
import qualified Data.Text               as Text

processImgUrlFetch ::  (Text.Text, Update) -> IO [SendMsg]
processImgUrlFetch (_, update) =
   case message_image_urls update of
     Just imgUrls' -> pure [makeReqFromUpdate update $ Text.pack $ concat imgUrls']
     _ -> pure [makeReqFromUpdate update "无效图片。"]
