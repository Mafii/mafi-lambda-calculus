module StepByStepReducer (reduce, Strategy (..), MaxSteps) where

import Lib (Term (Abs, App, Var))
import Reducer (betaReduce)

data Strategy = LeftmostOutermost | LeftmostInnermost
  deriving (Show)

type MaxSteps = Int

reduce :: Term -> Strategy -> MaxSteps -> Term
reduce term _ 0 = term
reduce term strategy steps = reduce (strategyF term) strategy (steps - 1)
  where
    strategyF = case strategy of
      LeftmostOutermost -> reduceOutermost1
      LeftmostInnermost -> reduceInnermost1

-- TOOD: I am not sure all of this is correct and/or complete :)

-- Some things to read and think about:
-- "If we restrict the normal order and applicative order strategies so as not to perform reductions inside the body of λ-abstractions, we obtain strategies known as call-by-name (CBN) and call-by-value (CBV), respectively."
-- Source: https://www.cs.cornell.edu/courses/cs6110/2018sp/lectures/lec04.pdf
-- CBN: Leftmost outermost strategy
-- CBV: Leftmost innermost strategy

-- Does leftmost outermost, one time
-- Description: A function's arguments are substituted into the body of a function before they are reduced. A function's arguments are reduced as often as they are needed.
-- Remark: In case a normal form exists, the leftmost outermost reduction strategy will find it.
reduceOutermost1 :: Term -> Term
reduceOutermost1 app@(App Abs {} _) = betaReduce app
reduceOutermost1 (App lhs@App {} rhs) = App (reduceOutermost1 lhs) rhs
reduceOutermost1 (App lhs rhs) = App lhs (reduceOutermost1 rhs)
reduceOutermost1 val = val

-- Does leftmost innermost, one time
-- Description: A function's arguments are substituted into the body of a functionj after they are reduced. A function's arguments are reduced exactly once.
-- Remark: In case a normal form exists, the leftmost innermost reduction strategy might not find it. It may never terminate.
reduceInnermost1 :: Term -> Term
reduceInnermost1 var@Var {} = var
reduceInnermost1 (App abs@Abs {} app@App {}) = App abs $ reduceInnermost1 app
reduceInnermost1 app@(App Abs {} _) = betaReduce app
reduceInnermost1 val = val