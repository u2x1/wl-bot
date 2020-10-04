{-# LANGUAGE OverloadedStrings #-}
module Module.GetImgUrl where

import           Core.Data.Unity
import           Core.Type.Unity.Request
import           Core.Type.Unity.Update
import qualified Data.Text               as Text

processImgUrlFetch ::  (Text.Text, Update) -> IO [SendMsg]
processImgUrlFetch (_, update) =
   case message_image_urls update of
     [] -> pure [makeReqFromUpdate update "未输入图片。"]
     imgUrls' -> pure [makeReqFromUpdate update $ Text.pack $ concat imgUrls']
