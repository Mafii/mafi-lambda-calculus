{-# LANGUAGE QuasiQuotes #-}

module Reducer (reduce) where

import LCQQ (lambda, λ)
import Lib (Id, Term (Abs, App, Var))

reduce :: Term -> Term
reduce a = a

-- "free variable" ^= variable that is captured from outer scope
-- "bound variable" ^= variable that is used inside abstraction and is bound by the abstraction variable id
-- e.g.:
-- - bound: λa.a <- a is bound variable
-- - free:  λa.b <- b is a free variable

-- todo: ensure no conflicts are happening
getFreeVariables :: Term -> [Id]
getFreeVariables (Abs id body)
  | id `isBoundIn` body = error "unhandled case, need to un-conflict variable first"
  | otherwise = filter (/= id) $ getFreeVariables body
getFreeVariables (Var id) = [id]
getFreeVariables (App lhs rhs) = getFreeVariables lhs ++ getFreeVariables rhs

-- >>> getFreeVariables [lambda| a b c d |]
-- >>> getFreeVariables [lambda| lambda a . lambda b . a c b |]
-- ["a","b","c","d"]
-- ["c"]

isFreeIn :: Id -> Term -> Bool
isFreeIn id term = elem id $ getFreeVariables term

-- >>> "c" `isFreeIn` [lambda| lambda a . lambda b . a c b |]
-- >>> "b" `isFreeIn` [lambda| lambda a . lambda b . a c b |]
-- True
-- False

isBoundIn :: Id -> Term -> Bool
isBoundIn id (Abs id' body)
  | id == id' = True
  | otherwise = id `isBoundIn` body
isBoundIn id (Var id') = False
isBoundIn id (App lhs rhs) = id `isBoundIn` lhs || id `isBoundIn` rhs

-- >>> "b" `isBoundIn` [lambda| lambda a . lambda b . a c b |]
-- >>> "c" `isBoundIn` [lambda| lambda a . lambda b . a c b |]
-- True
-- False

alphaConvert :: Term -> Id -> Term
alphaConvert (Abs oldId body) newId
  | newId `isFreeIn` body || newId `isBoundIn` body = error "new id must not be bound or free inside abstraction"
  | otherwise = Abs newId $ conv body oldId newId
  where
    conv (Abs id body) old new = Abs id (conv body old new)
    conv (App lhs rhs) old new = App (conv lhs old new) (conv rhs old new)
    conv (Var id) old new = if id == old then Var new else Var id
alphaConvert _ _ = error "can only alpha convert abstractions"

-- >>> [lambda| lambda a . lambda b . a c b |]
-- >>> alphaConvert it "x"
-- (λa. (λb. ((a c) b)))
-- (λx. (λb. ((x c) b)))

alphaEq :: Term -> Term -> Bool
alphaEq (Var id) (Var id') = id == id'
alphaEq (App lhs rhs) (App lhs' rhs') = lhs `alphaEq` lhs' && rhs `alphaEq` rhs'
alphaEq lhs@(Abs id body) rhs@(Abs id' body')
  | id == id' = body `alphaEq` body'
  | not (id' `isFreeIn` body) = alphaConvert lhs id' `alphaEq` rhs
  | not (id `isFreeIn` body') = alphaConvert rhs id `alphaEq` lhs
  | otherwise = error "need to pick random identifier to alpha convert lhs and rhs first, not yet done"
alphaEq _ _ = False

-- >>> alphaEq [λ| λ a . λ b . a c b |] [λ| λ x . λ b . x c b |]
-- >>> alphaEq [λ| λ a . x |] [λ| λ x . x |]
-- >>> alphaEq [λ| λ a . x |] [λ| λ x . a |]
-- True
-- False
-- need to pick random identifier to alpha convert lhs and rhs first, not yet done

betaConvert :: Term -> Term
betaConvert (App (Abs id body) rhs)
  | id `isFreeIn` rhs = error "Needs alpha conversion first to avoid variable conflicts in application"
  | id `isBoundIn` body = error "Precondition failure: Body of abstraction has bind with id of abstraction"
  | any (`isBoundIn` body) $ getFreeVariables rhs =
    error "Precondition failure: Rhs of reduction contains bound variable conflicting bind in lhs"
  | otherwise = replaceIdWithConcreteValue body id rhs
betaConvert _ = error "not yet implemeneted/defined behaviour"

-- precondition: no naming conflicts
replaceIdWithConcreteValue :: Term -> Id -> Term -> Term
replaceIdWithConcreteValue var@(Var id') id replacement
  | id == id' = replacement
  | otherwise = var
replaceIdWithConcreteValue (App lhs rhs) id replacement =
  App (replaceIdWithConcreteValue lhs id replacement) (replaceIdWithConcreteValue rhs id replacement)
replaceIdWithConcreteValue (Abs id' body) id replacement = Abs id' (replaceIdWithConcreteValue body id replacement)

-- >>> betaConvert [λ| (λ a . a) b |]
-- >>> betaConvert [λ| (λ a . a) (λ a . a) |]
-- >>> betaConvert [λ| (λ a . λ b . a) (99 * c) |]
-- >>> betaConvert [λ| (λ a . λ b . a) b |]
-- b
-- (λa. a)
-- (λb. ((99 *) c))
-- Precondition failure: Rhs of reduction contains bound variable conflicting bind in lhs
