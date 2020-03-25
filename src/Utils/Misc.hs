module Utils.Misc where

import Data.Text as Text
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Search  as BL

checkEmpty :: (Monoid a, Eq a) => a -> Maybe a
checkEmpty txt = if txt == mempty then Nothing else Just txt

searchBetweenBL :: BS.ByteString -> BS.ByteString -> BL.ByteString -> Maybe BL.ByteString
searchBetweenBL left right content =
  let fstround = snd $ BL.breakAfter left content in
      checkEmpty $ fst (BL.breakOn right fstround)

searchBetweenText :: Text -> Text -> Text -> Maybe Text
searchBetweenText left right content =
  let fstround = snd $ Text.breakOn left content in
      checkEmpty $ fst (Text.breakOn right fstround)
