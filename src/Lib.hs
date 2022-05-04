module Lib
  ( Id,
    Term (Var, App, Abs),
  )
where

type Id = String

data Term
  = Var Id -- Variables
  | Abs Id Term -- Abstractions
  | App Term Term -- Applications

-- not alpha eq by design, so we can test parser
instance Eq Term where
  (==) (Var lhs) (Var rhs) = lhs == rhs
  (==) (Abs id term) (Abs id' term') = term == term' && id == id'
  (==) (App lhs rhs) (App lhs' rhs') = lhs == lhs' && rhs == rhs'
  (==) _ _ = False

instance Show Term where
  show (Var v) = v
  show (Abs id term) = "(λ" ++ id ++ ". " ++ show term ++ ")"
  -- show (App term term') = show term ++ " " ++ show term'
  show (App term term') = "(" ++ show term ++ " " ++ show term' ++ ")" -- easier to argue about