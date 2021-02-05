{-# LANGUAGE OverloadedStrings #-}
module Module.GetImgUrl where

import           Core.Data.Unity                ( makeReqFromUpdate )
import           Core.Type.Unity.Request        ( SendMsg )
import           Core.Type.Unity.Update         ( Update(message_image_urls) )
import qualified Data.Text                     as Text

processImgUrlFetch :: (Text.Text, Update) -> IO [SendMsg]
processImgUrlFetch (_, update) = case message_image_urls update of
  []       -> pure [makeReqFromUpdate update "未输入图片。"]
  imgUrls' -> pure [makeReqFromUpdate update $ Text.pack $ concat imgUrls']
