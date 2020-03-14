module Utils.Misc where

import Data.Text as Text

checkEmpty :: Text -> Maybe Text
checkEmpty txt = if txt == mempty then Nothing else Just txt
