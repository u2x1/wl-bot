{-# LANGUAGE FlexibleInstances #-}
module Core.Type.EitherT where

import           Core.Data.Unity
import           Core.Type.Unity.Request
import           Core.Type.Unity.Update
import           Data.Text                      ( Text )

newtype MEitherT a = MEitherT { runMEitherT :: IO (Either Text a) }

instance Functor MEitherT where
  fmap = (<$>)

instance Applicative MEitherT where
  pure  = MEitherT . pure . Right
  (<*>) = (<*>)

instance Monad MEitherT where
  x >>= f = MEitherT $ do
    rx <- runMEitherT x
    case rx of
      Left  err -> return $ Left err
      Right a   -> runMEitherT $ f a

lift :: IO a -> MEitherT a
lift = MEitherT . fmap Right

liftList :: Text -> IO [a] -> MEitherT [a]
liftList t x = do
  listx <- lift x
  case listx of
    [] -> MEitherT $ pure $ Left t
    y  -> pure y

liftMaybe :: Text -> IO (Maybe a) -> MEitherT a
liftMaybe t x = do
  maybex <- lift x
  case maybex of
    Nothing    -> MEitherT $ pure $ Left t
    Just justx -> pure justx

liftEither :: (e -> Text) -> IO (Either e a) -> MEitherT a
liftEither f x = do
  eitherx <- lift x
  case eitherx of
    Left  t      -> MEitherT $ pure $ Left (f t)
    Right rightx -> pure rightx

getTextT :: Either Text Text -> Text
getTextT (Right x) = x
getTextT (Left  x) = x

getTextT' :: Update -> Either Text [SendMsg] -> [SendMsg]
getTextT' _      (Right x) = x
getTextT' update (Left  x) = [makeReqFromUpdate update x]
