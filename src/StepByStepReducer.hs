-- for comment evaluation during development
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module StepByStepReducer (reduce, ReduceInstruction (..), Strategy (..), Steps) where

import GHC.Base (undefined)
import LCQQ (lambda, λ)
import Lib (Id, Term (Abs, App, Var))
import Reducer (betaReduce)

data Strategy = LeftmostOutermost | LeftmostInnermost
  deriving (Show)

type Steps = Int

data ReduceInstruction = ReduceInstruction Strategy Steps
  deriving (Show)

reduce :: Term -> ReduceInstruction -> Term
reduce term (ReduceInstruction _ 0) = term
reduce _ (ReduceInstruction LeftmostInnermost _) = undefined "not done"
reduce term (ReduceInstruction LeftmostOutermost steps) = reduce (reduce1 term) (ReduceInstruction LeftmostOutermost (steps - 1))

-- Does leftmost outermost
-- Remark: In case a normal form exists, the leftmost outermost reduction strategy will find it.
reduce1 :: Term -> Term
reduce1 var@Var {} = var
reduce1 (Abs id term) = Abs id (reduce1 term)
reduce1 app@(App (Abs {}) _) = betaReduce app
reduce1 (App lhs rhs) = App (reduce1 lhs) rhs

-- >>> True
-- True
