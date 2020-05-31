{-# LANGUAGE OverloadedStrings #-}
module Core.Data.Mirai where

import           Core.Type.Mirai.Update  as MU (MRMsg, mrm_url, mrm_type, mrm_text)
import           Core.Type.Unity.Request as UR (text, imgUrl, imgPath, SendMsg)
import           Core.Type.Mirai.Request as MR (Message (Message))
import           HTMLEntities.Decoder          (htmlEncodedText)
import           Data.Text                     (pack)
import           Data.Text.Lazy                (toStrict)
import           Data.Text.Lazy.Builder        (toLazyText)
import           Data.Maybe                    (catMaybes, fromJust)

-- | Get a list of image urls from Mirai messages.
getImgUrls :: [MRMsg] -> Maybe [String]
getImgUrls cqMsgs = let x = catMaybes (mrm_url <$> filter (\m -> mrm_type m == "Image") cqMsgs) in
                       if null x then Nothing else Just x

-- | Get plain text from Mirai messages.
getText :: [MRMsg] -> Maybe String
getText cqMsgs = let x = fromJust.mrm_text <$> filter (\m -> mrm_type m == "Plain") cqMsgs in
                     if null x then Nothing else Just $ mconcat x

-- | Transform Unity SendMsg to Mirai Messages.
transMsg :: UR.SendMsg -> [Message]
transMsg m = fromText (UR.text m) <> fromImgUrl (UR.imgUrl m) <> fromImgPath (UR.imgPath m)
  where
    fromText t = case t of
        Just txt -> [Message "Plain" (Just (toStrict.toLazyText.htmlEncodedText $ txt)) Nothing Nothing]
        _ -> []

    fromImgUrl imgs = case imgs of
        Just url -> [Message "Image" Nothing (Just url) Nothing]
        _ -> []

    fromImgPath imgs = case pack <$> imgs of
        Just path -> [Message "Image" Nothing Nothing (Just path)]
        _ -> []
