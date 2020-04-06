{-# LANGUAGE OverloadedStrings #-}
module Core.Data.Mirai where

import           Core.Type.Mirai.Update
import qualified Core.Type.Unity.Request as UN
import           Core.Type.Mirai.Request

import qualified Data.Text                      as Text
import           Data.Maybe

getImgUrls :: [MRMsg] -> Maybe [Text.Text]
getImgUrls cqMsgs = let x = catMaybes (mrm_url <$> filter (\m -> mrm_type m == "Image") cqMsgs) in
                       if null x then Nothing else Just x

getText :: [MRMsg] -> Maybe Text.Text
getText cqMsgs = let x = fromJust.mrm_text <$> filter (\m -> mrm_type m == "Plain") cqMsgs in
                     if null x then Nothing else Just $ mconcat x

transMsg :: UN.SendMsg -> [Message]
transMsg smsg = (fromText (UN.text smsg)) <> (fromImg (UN.imgUrls smsg))
  where fromText t = case t of
                       Just txt -> [Message "Plain" (Just txt) Nothing]
                       _ -> []
        fromImg imgs = case imgs of
                         Just urls -> fmap (Message "Image" Nothing . Just) urls
                         _ -> []

data MiraiDataMsg = ImageUrl Text.Text | Text Text.Text
