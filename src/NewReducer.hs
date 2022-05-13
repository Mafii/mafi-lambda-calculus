{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module NewReducer where

import GHC.Base (undefined)
import LCQQ (lambda, λ)
import Lib (Id, Term (Abs, App, Var))

data Strategy = LeftmostOutermost | LeftmostInnermost
  deriving (Show)

type Steps = Int

data ReduceInstruction = ReduceInstruction Strategy Steps
  deriving (Show)

reduce :: Term -> ReduceInstruction -> Term
reduce term (ReduceInstruction LeftmostOutermost steps) = term
reduce term (ReduceInstruction LeftmostInnermost steps) = term
