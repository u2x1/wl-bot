{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Utils.Json where

import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics

dropToJSON :: forall a.(Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
dropToJSON prefix = genericToJSON defaultOptions {
    fieldLabelModifier = drop prefix
  , omitNothingFields = True
  }

dropParseJSON :: forall a.(Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
dropParseJSON prefix = genericParseJSON defaultOptions { fieldLabelModifier = drop prefix }
