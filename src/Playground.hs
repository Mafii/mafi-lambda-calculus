{-# LANGUAGE QuasiQuotes #-}

module Playground where

import Control.Applicative (Applicative (liftA2))
import LCQQ (lambda, λ)

infixr 0 |>

(|>) :: b -> (b -> c) -> c
(|>) = flip ($)

(|||) :: (Int -> Bool) -> (Int -> Bool) -> Int -> Bool
(|||) = liftA2 (||)

f' :: b1 -> b2 -> (b1 -> b2 -> c) -> c
f' a b c = b |> a |> c

f :: Int -> Bool
f a = y a |> g ||| h

g :: p -> Bool
g a = True

h :: p -> Bool
h a = False

y a = a

-- (a b) (c d)

-- (a) ((λ a . a a) (2 2))
-- App (App (Var "2") (Var "2")) (App (Var "2") (Var "2"))
--

-- a b c
-- (a b) c

-- `(λx.λy.x y) y` reduces to `λy1.y y1`, and not `λy.y y`
-- reduce (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")) == (Abs "y1" (App (Var "y") (Var "y1")))

-- (λy1.y y1) == (λabc.y abc)

-- >>> :t [lambda| (λ a . a) |]
-- [lambda| (λ a . a) |] :: Term

-- abc λ a . a b c d

-- void Test(int i)

fn :: p -> p
fn a = a

---(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
---(|||) = liftA2 (||)

-- >>> :t not . ((> 3) ||| (> 2))
-- >>> not . ((> 3) ||| (> 2)) $ 7
-- not . ((> 3) ||| (> 2)) :: (Ord a, Num a) => a -> Bool
-- False

--test :: Bool
--test = (|||) (> 3) (> 2) 1

--test' :: Bool
--test' = (|||) (> 3) (> 2) 4

-- >>> test
-- >>> test'
-- False
-- True