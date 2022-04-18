module Lib
  ( Id,
    Term,
  )
where

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
  (==) (Abs id term) (Abs id' term') = term == replaceVariableName id' term' id
  (==) (App lhs rhs) (App lhs' rhs') = lhs == lhs' && rhs == rhs'
  (==) _ _ = False

-- `(λx.x x) (λx.x)` reduces to `λx.x`
-- >>> reduce (App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (Var "x"))) == (Abs "x" (Var "x"))
-- False

instance Show Term where
  show (Var v) = "(Var " ++ v ++ ")"
  show (Abs id term) = "(Abs λ" ++ id ++ ". " ++ show term ++ ")"
  show (App term term') = "(App " ++ show term ++ " " ++ show term' ++ ")"

-- >>> show (Var "x")
-- "(Var x)"

-- >>> show $ reduce (App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (Var "x")))
-- "(App (Abs \955x. (Var x)) (Abs \955x. (Var x)))"

betaReduce :: Term -> Term
betaReduce = undefined

deltaReduce :: Term -> Term
deltaReduce = undefined

-- >>> betaReduce Zero