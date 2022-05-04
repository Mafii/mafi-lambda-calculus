module Playground where

import Control.Applicative (Applicative (liftA2))

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = liftA2 (||)

-- >>> :t not . ((> 3) ||| (> 2))
-- >>> not . ((> 3) ||| (> 2)) $ 7
-- not . ((> 3) ||| (> 2)) :: (Ord a, Num a) => a -> Bool
-- False

test :: Bool
test = (|||) (> 3) (> 2) 1

test' :: Bool
test' = (|||) (> 3) (> 2) 4

-- >>> test
-- >>> test'
-- False
-- True
