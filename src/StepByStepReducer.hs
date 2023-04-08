module StepByStepReducer (reduce, Strategy (..), Steps) where

import Lib (Term (Abs, App, Var))
import Reducer (betaReduce)

data Strategy = LeftmostOutermost | LeftmostInnermost
  deriving (Show)

type Steps = Int

reduce :: Term -> Strategy -> Steps -> Term
reduce term _ 0 = term
reduce _ LeftmostInnermost _ = undefined "not done"
reduce term LeftmostOutermost steps = reduce (reduce1 term) LeftmostOutermost (steps - 1)

-- Does leftmost outermost
-- Remark: In case a normal form exists, the leftmost outermost reduction strategy will find it.
reduce1 :: Term -> Term
reduce1 var@Var {} = var
reduce1 (Abs id term) = Abs id (reduce1 term)
reduce1 app@(App (Abs {}) _) = betaReduce app
reduce1 (App lhs rhs) = App (reduce1 lhs) rhs
