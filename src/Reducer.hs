-- required so comments can be evaluated
{-# LANGUAGE QuasiQuotes #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Reducer (reduce, alphaEq) where

import Data.List (nub)
import LCQQ (lambda, λ)
import Lib (Id, Term (Abs, App, Var))

reduce :: Term -> Term
reduce var@(Var id) = var
reduce app@(App (Abs id body) rhs) = reduce $ betaReduce app
reduce val = val

-- "free variable" ^= variable that is captured from outer scope
-- "bound variable" ^= variable that is used inside abstraction and is bound by the abstraction variable id
-- e.g.:
-- - bound: λa.a <- a is bound variable
-- - free:  λa.b <- b is a free variable

getFreeVariables :: Term -> [Id]
getFreeVariables term = get term []
  where
    get term knownBinds
      | (Abs id body) <- term = get body $ id : knownBinds
      | (App lhs rhs) <- term = get lhs knownBinds ++ get rhs knownBinds
      | (Var id) <- term = if id `elem` knownBinds then [] else [id]

-- >>> getFreeVariables [lambda| a b c d |]
-- >>> getFreeVariables [lambda| lambda a . lambda b . a c b |]
-- ["a","b","c","d"]
-- ["c"]

isFreeIn :: Id -> Term -> Bool
isFreeIn id term = elem id $ getFreeVariables term

-- >>> "c" `isFreeIn` [lambda| lambda a . lambda b . a c b |]
-- >>> "a" `isFreeIn` [lambda| lambda a . lambda b . a c b |]
-- >>> "b" `isFreeIn` [lambda| lambda a . lambda b . a c b |]
-- True
-- False
-- False

getBoundVariables :: Term -> [Id]
getBoundVariables (Abs id body) = id : getBoundVariables body
getBoundVariables (Var id) = []
getBoundVariables (App lhs rhs) = getBoundVariables lhs ++ getBoundVariables rhs

-- >>> getBoundVariables [lambda| lambda a . y lambda x . 5 7 lambda 55 . a 5 |]
-- ["a","x","55"]

-- isBoundIn :: Id -> Term -> Bool
-- isBoundIn id (Abs id' body)
--   | id == id' = True
--   | otherwise = id `isBoundIn` body
-- isBoundIn id (Var id') = False
-- isBoundIn id (App lhs rhs) = id `isBoundIn` lhs || id `isBoundIn` rhs
isBoundIn :: Id -> Term -> Bool
isBoundIn id term = elem id $ getBoundVariables term

-- >>> "b" `isBoundIn` [lambda| lambda a . lambda b . a c b |]
-- >>> "c" `isBoundIn` [lambda| lambda a . lambda b . a c b |]
-- True
-- False

alphaConvert :: Term -> Id -> Term
alphaConvert (Abs oldId body) newId
  | newId `isFreeIn` body || newId `isBoundIn` body =
    error $ "new id must not be bound or free inside abstraction: " ++ show oldId ++ show newId ++ show body
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
  | not (id' `isFreeIn` body || id' `isBoundIn` body) = alphaConvert lhs id' `alphaEq` rhs
  | not (id `isFreeIn` body' || id `isBoundIn` body') = alphaConvert rhs id `alphaEq` lhs
  | otherwise = do
    let newId = getNonConflictingIdentifier lhs rhs id
    alphaConvert lhs newId `alphaEq` alphaConvert rhs newId
alphaEq _ _ = False

getNonConflictingIdentifier :: Term -> Term -> Id -> Id
getNonConflictingIdentifier term term' identifierBase = do
  let freeAndBoundVariables =
        getFreeVariables term ++ getBoundVariables term ++ getFreeVariables term' ++ getBoundVariables term'
  let number = until (not . flip elem freeAndBoundVariables . (identifierBase ++) . show) (+ 1) 1
  identifierBase ++ show number

-- >>> getNonConflictingIdentifier [λ| λ a . x |] [λ| λ x . a |] "a"
-- >>> getNonConflictingIdentifier [λ| λ a . x a4 a7 |] [λ| λ x . a a1 a2 a3 a5 |] "a"
-- "a1"
-- "a6"

-- >>> alphaEq [λ| λ a . λ b . a c b |] [λ| λ x . λ b . x c b |]
-- >>> alphaEq [λ| λ a . x |] [λ| λ x . x |]
-- >>> alphaEq [λ| λ a . x |] [λ| λ x . a |]
-- >>> alphaEq [λ| λ x . y |] [λ| λ y . y |]
-- >>> getNonConflictingIdentifier [λ| λy.λx.y |] [λ| λx.λy.x |] "y"
-- >>> alphaEq [λ| λy.λx.y |] [λ| λx.λy.x |]
-- True
-- False
-- False
-- False
-- "y1"
-- True

betaReduce :: Term -> Term
betaReduce app@(App (Abs id body) rhs) = do
  let fixed = replaceBindsConflictingWithFreeVariables app
  let newId = getId fixed -- could be improved with deconstruction in fixed: let (newId, newBody, fixedRhs) = ...
  let newBody = getBody fixed
  let fixedRhs = getRhs fixed
  replaceIdWithConcreteValue newBody newId fixedRhs
  where
    getId (App (Abs id body) _) = id
    getId _ = error "invalid usecase"
    getBody (App (Abs id body) _) = body
    getBody _ = error "invalid usecase"
    getRhs (App (Abs {}) rhs) = rhs
    getRhs _ = error "invalid usecase"
betaReduce _ = error "not yet implemeneted/defined behaviour"

replaceBindsConflictingWithFreeVariables :: Term -> Term
replaceBindsConflictingWithFreeVariables (App abs@(Abs id body) rhs) = do
  let freeVars = getFreeVariables rhs ++ getFreeVariables abs
  let newIdSelector oldId = if oldId `elem` freeVars then getNonConflictingIdentifier abs rhs oldId else oldId
  let fixedBody = replaceConflicts body newIdSelector
  let fixedRhs = replaceConflicts rhs newIdSelector
  App (Abs (newIdSelector id) fixedBody) rhs
  where
    replaceConflicts term f -- f is the new id factory
      | (Abs id body) <- term = createAbs id f body
      | (App lhs rhs) <- term = App (replaceConflicts lhs f) (replaceConflicts rhs f)
      | var@(Var id) <- term = var
    createAbs :: Id -> (Id -> Id) -> Term -> Term
    createAbs oldId newIdFactory body = do
      let newId = newIdFactory oldId
      let newBody = replaceIdWithConcreteValue body oldId (Var newId)
      Abs newId newBody
replaceBindsConflictingWithFreeVariables _ = error "unhandled"

-- >>> betaReduce [λ| (λ a . λ b . a b) b |]
-- (λb1. (b b1))

-- precondition: no naming conflicts
replaceIdWithConcreteValue :: Term -> Id -> Term -> Term
replaceIdWithConcreteValue var@(Var id') id replacement
  | id == id' = replacement
  | otherwise = var
replaceIdWithConcreteValue (App lhs rhs) id replacement =
  App (replaceIdWithConcreteValue lhs id replacement) (replaceIdWithConcreteValue rhs id replacement)
replaceIdWithConcreteValue (Abs id' body) id replacement = Abs id' (replaceIdWithConcreteValue body id replacement)

-- >>> betaReduce [λ| (λ a . a) |]
-- >>> betaReduce [λ| (λ a . a) b |]
-- >>> betaReduce [λ| (λ a . a a a) b |]
-- >>> betaReduce [λ| (λ a . a) (λ a . a) |]
-- >>> betaReduce [λ| (λ a . λ b . a) (99 * c) |]
-- >>> betaReduce [λ| (λ a . λ b . a) b |]
-- not yet implemeneted/defined behaviour
-- b
-- ((b b) b)
-- (λa. a)
-- (λb. ((99 *) c))
-- (λb1. b)
