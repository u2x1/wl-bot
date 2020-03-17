{-# LANGUAGE OverloadedStrings #-}
module Core.Data.CoolQ where

import Core.Type.CoolQ.Update         as CQ

import Data.Text              as T
import Data.Maybe

getImgUrls :: [CQMsg] -> [Text]
getImgUrls (x:cqMsg) = case cqtype x of
                        "image" -> fromJust (url $ cqdata x) : getImgUrls cqMsg
                        _       -> getImgUrls cqMsg
getImgUrls [] = []

getText :: [CQMsg] -> Text
getText msg = go msg 0
  where
    go :: [CQMsg] -> Int -> Text
    go [] _ = ""
    go (x:cqMsg) picCount = case cqtype x of
      "text"  -> fromJust (CQ.text $ cqdata x)              <> go cqMsg picCount
      "image" -> T.concat ["[P", pack (show picCount), "]"] <> go cqMsg (picCount + 1)
      _       -> "[[Unsupported Message]]"                  <> go cqMsg picCount
