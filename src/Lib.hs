-- {-# LANGUAGE FlexibleContexts #-}

module Lib
  ( Id,
    Term (Var, App, Abs),
  )
where

import GHC.Base (Symbol)
import Prelude hiding ((.))

type Id = String

data Term
  = Var Id -- Variables
  | Abs Id Term -- Abstractions
  | App Term Term -- Applications

combine :: Id -> Id -> Id
combine a b = a ++ b

reduce :: Term -> Term
reduce (Var id) = Var id
reduce (Abs id t) = Abs id t
reduce (App lhs rhs) = apply lhs rhs

apply :: Term -> Term -> Term
apply (Abs boundVar term) = replace term boundVar
apply _ = error "unhandled"

replace :: Term -> Id -> Term -> Term
replace term var replacement = case term of
  Var id -> if id == var then replacement else Var id
  Abs id term -> do
    let nonConflictingTerm = fixVariableNameConflicts var (Abs id term)
    let newTerm = replace term id replacement
    newTerm
  App term term' -> App (replace term var replacement) (replace term' var replacement)

fixVariableNameConflicts :: Id -> Term -> Term
fixVariableNameConflicts outer (Abs id term) =
  if outer == id
    then replaceVariableName id term (id `combine` "'")
    else Abs id term
fixVariableNameConflicts _ _ = error "unhandled"

replaceVariableName :: Id -> Term -> Id -> Term
replaceVariableName id term newId
  | Var s <- term = if s == id then Var newId else term
  | Abs id' term' <- term = do
    let new = if id == id' then replaceVariableName id term' (id `combine` "'") else term'
    replaceVariableName id term' newId
  | App lhs rhs <- term = App (replaceVariableName id lhs newId) (replaceVariableName id rhs newId)

-- `(λx.x) a` reduces to `a`
-- >>> reduce (App (Abs "x" (Var "x")) (Var "a")) == (Var "a")
-- True

-- >>> reduce  (App (Abs "x" (Var "x")) (Var "b")) == (Var "a")
-- False

instance Eq Term where
  (==) (Var lhs) (Var rhs) = lhs == rhs
  (==) (Abs id term) (Abs id' term') = term == term' && id == id'
  --(==) (Abs id term) (Abs id' term') = term == replaceVariableName id' term' id -- ensure alpha equivalence (semi-todo)
  (==) (App lhs rhs) (App lhs' rhs') = lhs == lhs' && rhs == rhs'
  (==) _ _ = False

-- `(λx.x x) (λx.x)` reduces to `λx.x`
-- >>> reduce (App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (Var "x"))) == (Abs "x" (Var "x"))
-- False

instance Show Term where
  show (Var v) = v
  show (Abs id term) = "(λ" ++ id ++ ". " ++ show term ++ ")"
  show (App term term') = show term ++ " " ++ show term'

-- >>> (Var "x")
-- >>> (Abs "y" (Var "y"))
-- >>> App it it
-- >>> reduce it
-- x
-- (λy. y)
-- (App (λy. y) (λy. y))
-- (λy. y)

-- >>> reduce (App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (Var "x")))
-- (App (λx. x) (λx. x))

-- application of parameter to function,
-- replacing the bound variable with the given parameter inside the term of the function
betaReduce :: Term -> Term
betaReduce (App lhs rhs) = apply lhs
  where
    apply (Abs id term) = replace term id rhs
    apply _ = error "not yet defined"
betaReduce _ = error "not yet defined"

-- substitution of a defined symbol with its definition
deltaReduce :: Term -> Term
deltaReduce = undefined

-- >>> (Var "x")
-- >>> (Abs "y" (Var "y"))
-- >>> App it it
-- >>> betaReduce it
-- x
-- (λy. y)
-- (App (λy. y) (λy. y))
-- (λy. y)

-- >>> (App (Abs "y" (App (Var "Square") (Var "y"))) (Var "5"))
-- >>> betaReduce it
-- (App (λy. (App Square y)) 5)
-- (App Square 5)

-- dsl attempt
λ :: Id -> Term -> Term
λ = Abs

lambda :: Id -> Term -> Term
lambda = λ

infix 8 . -- makes sure … is evaluated first

-- only works because I hid the (.) operator from the prelude
-- takes partially applied λ and a term
(.) :: (Term -> Term) -> Term -> Term
(.) absCreator = absCreator

infix 9 …

(…) :: String -> String -> Term
(…) id id' = App (Var id) (Var id')

-- >>> :t "a" … "b"
-- "a" … "b" :: Term

-- >>> lambda "x". "a"…"b"
-- (λx. a b)

-- >>> (λ"x". "a"…"b")
-- (λx. a b)

{-
-- (.) of lc taking pair of ids for easier inline coding turining them to vars/apps
(…) :: (Term -> Term) -> (String -> String -> Term)
(…) absCreator id id' = absCreator (App (Var id) (Var id'))

v :: Id -> Term
v = Var

-- >>> λ "x" . (Var "x")
-- (λx. x)

-- >>> lambda "x" . (App (Var "x") (Var "x"))
-- (λx. x x)

-- >>> lambda "x" … v"y" v"y"
-- Couldn't match expected type ‘(Id -> Term) -> String -> Id’
--             with actual type ‘Term’

-- >>> App (λ"x"… "square" "x") ((λ"y"… "square" "y") "5")

-- >>> (lambda "x" .) (v"a")
-- (λx. a)

-- >>> (lambda "x" …) "a" "b"
-- (λx. a b)

(|>) :: b -> (b -> c) -> c
(|>) = flip ($)

-- >>> :t ($)
-- ($) :: (a -> b) -> a -> b

-- >>> (+) 3 $ (+) 2 3

-- >>> :t ((5 +) |>)
-- >>> ((5 +) |>) (\x -> x) 5
-- ((5 +) |>) :: Num a => ((a -> a) -> c) -> c
-- 10

-- >>> ((5 +) |>) id 5
-- 10

(|>>) :: (t1 -> t2) -> t1 -> t2
(|>>) a = a |> id

-- >>> :t ((5 +) |>>)
-- >>> ((5 +) |>>) (5)
-- ((5 +) |>>) :: Num t2 => t2 -> t2
-- 10

-- >>> (+) 5 |>> 5
-- 10

-- >>> lambda "x" |>> v"y"
-- (λx. y)

-- >>> lambda "y" |>> (App  (v"y") $ v"x")
-- (λy. y x)

(>>>) :: String -> (Id -> Term) -> Term
(>>>) id constructor = constructor id

-- >>> "a" >>> Var
-- a

-- >>> lambda "x" . ("a" >>> Var)
-- (λx. a)

-}
