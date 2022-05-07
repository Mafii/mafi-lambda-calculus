module ResultMonad (R (Ok, Error), fromOk) where

import Control.Applicative (Alternative)
import GHC.Base (Alternative (empty, (<|>)), Applicative (liftA2), MonadPlus)

data R a
  = Ok a
  | Error String

instance Functor R where
  fmap f (Ok a) = pure $ f a
  fmap f (Error e) = Error e

instance Applicative R where
  pure = Ok
  liftA2 f ra rb = ra >>= \a -> rb >>= \b -> pure $ f a b -- actually <*> would've been easier but just as an exercise

instance Monad R where
  (>>=) (Ok a) f = f a
  (>>=) (Error e) f = Error e

instance (Show a) => Show (R a) where
  show (Error e) = e
  show (Ok value) = show value

instance Alternative R where
  empty = Error "uninitialized result monad"
  (Error e) <|> (Ok a) = Ok a
  (Ok a) <|> _ = Ok a
  (Error e) <|> (Error _) = Error e

instance MonadPlus R

-- >>> Ok "2" >>= return . (++ "2")
-- >>> Error "2" >>= return . (++ "2")
-- "22"
-- 2

-- just like fromJust of Maybe
fromOk :: R a -> a
fromOk (Ok a) = a
fromOk (Error e) = error $ "R.fromOk: " ++ e
