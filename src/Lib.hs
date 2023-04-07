module Lib (Id, Term (Var, App, Abs), showPretty) where

type Id = String

data Term
  = Var Id
  | Abs Id Term
  | App Term Term

-- not alpha eqivalence by choice
instance Eq Term where
  (==) (Var lhs) (Var rhs) = lhs == rhs
  (==) (Abs id term) (Abs id' term') = term == term' && id == id'
  (==) (App lhs rhs) (App lhs' rhs') = lhs == lhs' && rhs == rhs'
  (==) _ _ = False

instance Show Term where
  show (Var v) = v
  show (Abs id term) = "(λ" ++ id ++ ". " ++ show term ++ ")"
  show (App term term') = "(" ++ show term ++ " " ++ show term' ++ ")" -- easier to argue about than without parenthesis 

showPretty :: Term -> String
showPretty (Var v) = v
showPretty (Abs id term) = "λ" ++ id ++ "." ++ showPretty term
showPretty (App (Abs id body) term') = "(λ" ++ id ++ "." ++ showPretty body ++ ") " ++ showPretty term'
showPretty (App (Var id) rhs@(App {})) = id ++ " (" ++ showPretty rhs ++ ")"
showPretty (App term term') = showPretty term ++ " " ++ showPretty term'
