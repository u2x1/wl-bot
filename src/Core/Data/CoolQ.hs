{-# LANGUAGE OverloadedStrings #-}
module Core.Data.CoolQ where

import           Core.Type.CoolQ.Update         as CQ

import qualified Data.Text                      as Text
import           Data.Maybe

getImgUrls :: [CQMsg] -> Maybe [Text.Text]
getImgUrls cqMsgs = let x = fromJust.url.cqdata <$> filter (\m -> cqtype m == "image") cqMsgs in
                        if null x then Nothing else Just x

getText :: [CQMsg] -> Maybe Text.Text
getText cqMsgs = let x = fromJust.text.cqdata <$> filter (\m -> cqtype m == "text") cqMsgs in
                     if null x then Nothing else Just $ mconcat x
--   where
--     go :: [CQMsg] -> Int -> Text.Text
--     go [] _ = ""
--     go (x:cqMsg) picCount = case cqtype x of
--       "text"  -> fromJust (CQ.text $ cqdata x)              <> go cqMsg picCount
--       "image" -> mconcat ["[P", Text.pack (show picCount), "]"]  <> go cqMsg (picCount + 1)
--       _       -> "[[Unsupported Message]]"                  <> go cqMsg picCount
