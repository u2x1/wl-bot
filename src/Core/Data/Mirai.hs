{-# LANGUAGE OverloadedStrings #-}
module Core.Data.Mirai where

import           Core.Type.Mirai.Update
import qualified Core.Type.Unity.Request as UN
import           Core.Type.Mirai.Request

import qualified Data.Text                      as Text
import           Data.Maybe

getImgUrls :: [MRMsg] -> Maybe [String]
getImgUrls cqMsgs = let x = catMaybes (mrm_url <$> filter (\m -> mrm_type m == "Image") cqMsgs) in
                       if null x then Nothing else Just x

getText :: [MRMsg] -> Maybe String
getText cqMsgs = let x = fromJust.mrm_text <$> filter (\m -> mrm_type m == "Plain") cqMsgs in
                     if null x then Nothing else Just $ mconcat x

transMsg :: UN.SendMsg -> [Message]
transMsg m = fromText (UN.text m) <> fromImgUrl (UN.imgUrl m) <> fromImgPath (UN.imgPath m)
  where fromText t = case t of
                       Just txt -> [Message "Plain" (Just txt) Nothing Nothing]
                       _ -> []

        fromImgUrl imgs = case imgs of
                            Just url -> [Message "Image" Nothing (Just url) Nothing]
                            _ -> []

        fromImgPath imgs = case Text.pack <$> imgs of
                             Just path -> [Message "Image" Nothing Nothing (Just path)]
                             _ -> []
