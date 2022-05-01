module Parser (parse) where

import Data.Either (fromLeft)
import qualified Data.Foldable
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Lib (Term)
import qualified Lib
import qualified Tokenizer
  ( Token
      ( ClosingParenthesis,
        FunctionAbstractionDot,
        LambdaCharacter,
        LambdaWord,
        Newline,
        OpeningParenthesis,
        Space,
        VariableUsageOrBinding
      ),
    tokenize,
  )

data Token
  = OpenParens
  | Dot
  | Lambda
  | ClosingParens
  | VarUseOrBind String
  deriving (Show)

flatMapTokens :: [Tokenizer.Token] -> [Token]
flatMapTokens = flatMap mapToken
  where
    flatMap f = concatMap (Data.Foldable.toList . f)

mapToken :: Tokenizer.Token -> Maybe Token
mapToken Tokenizer.OpeningParenthesis = Just OpenParens
mapToken Tokenizer.ClosingParenthesis = Just ClosingParens
mapToken Tokenizer.FunctionAbstractionDot = Just Dot
mapToken Tokenizer.LambdaCharacter = Just Lambda
mapToken Tokenizer.LambdaWord = Just Lambda
mapToken Tokenizer.Newline = Nothing
mapToken Tokenizer.Space = Nothing
mapToken (Tokenizer.VariableUsageOrBinding id) = Just $ VarUseOrBind id

data TokenOrScope
  = T Token
  | Scope [TokenOrScope]
  deriving (Show)

data IntermediateParseResult
  = Var String
  | Abs String (Maybe (Either CompletedParseResult IntermediateParseResult))
  | App IntermediateParseResult (Either CompletedParseResult IntermediateParseResult)
  | AppPC CompletedParseResult (Either CompletedParseResult IntermediateParseResult)
  deriving (Show)

data CompletedParseResult
  = VarC String
  | AbsC String CompletedParseResult
  | AppC CompletedParseResult CompletedParseResult
  deriving (Show)

data Extendable = E IntermediateParseResult [TokenOrScope] | Closed (Either CompletedParseResult IntermediateParseResult) [TokenOrScope]
  deriving (Show)

data ParseResult
  = Completed CompletedParseResult
  | Extendable Extendable
  | Intermediate IntermediateParseResult
  | Unhandled [TokenOrScope]
  | Error String
  deriving (Show)

parse :: [Tokenizer.Token] -> Term
parse tokens =
  toTerm $
    ensureCompleted $
      parse' $
        Unhandled $
          getScopes $
            generalize $
              flatMapTokens tokens

generalize :: [Token] -> [TokenOrScope]
generalize = map T

-- >>> parse $ Tokenizer.tokenize "(a(b)c)"
-- ((a b) c)

parse' :: ParseResult -> ParseResult
parse' inp | trace ("parse': " ++ show inp) False = undefined
parse' u@(Unhandled tokens) = unpack $ until isCompletedOrError extend (getFirstElement tokens)
-- parse' u@(Unhandled tokens) = do
--   let result = getFirstElement tokens
--   let tresult = trace (show (original result)) result
--   if isCompletedOrError result then unpack tresult else error "unfinished"
-- parse' p@(Extendable (E extendable unhandled)) = undefined -- unpack $ until isCompletedOrError extend (appExtender p)
-- parse' c@(Completed {}) = c
-- parse' e@(Error {}) = e
parse' val = error $ "could not parse: " ++ show val

-- extend

data Extender = Extender
  { original :: ParseResult,
    extendf :: Either CompletedParseResult IntermediateParseResult -> [TokenOrScope] -> ParseResult
  }

unpack :: Extender -> ParseResult
-- unpack val | trace ("unpack: " ++ show (original val)) False = undefined
unpack (Extender (Extendable (Closed el tks)) f) = Extendable (Closed el tks)
unpack (Extender (Extendable (E el tks)) f) = Extendable (E el tks)
unpack (Extender (Completed el) f) = Completed el
unpack val = error $ "unhandled unpack" ++ show (original val)

extend :: Extender -> Extender
extend val | trace ("extend: " ++ show (original val)) False = undefined
extend input = do
  let (r, rest, f) = case input of
        Extender (Extendable (E interm rest)) f -> (Right interm, rest, f)
        Extender (Extendable (Closed interm rest)) f -> (interm, rest, f)
        Extender (Intermediate interm) f -> (Right interm, [], \x y -> error "unhandled blub")
        Extender (Completed el) f -> (Left el, [], f)
        Extender a b -> error $ "unhandled extr: " ++ show a
  let next = if null rest then noopExtender r else getFirstElement rest
  let (newEl, newRest) = case next of
        Extender (Extendable (E el rest)) _ -> (Right el, rest)
        Extender (Extendable (Closed (Right el) rest)) f -> (Right el, rest)
        Extender (Extendable (Closed (Left el) rest)) f -> (Left el, rest)
        Extender (Completed el) f -> (Left el, [])
        Extender (Intermediate i) _ -> (Right i, [])
        val -> error $ "unhandled extnext" ++ show (original val)
  let result = case r of
        (Left lhs) -> case newEl of
          (Left rhs) -> Extendable (Closed (Left $ AppC lhs rhs) newRest)
          (Right rhs) -> f (Right rhs) newRest
        (Right lhs) -> case newEl of
          (Left rhs) -> Extendable (E (App lhs (Left rhs)) newRest)
          (Right rhs) -> f (Right rhs) newRest
  let extender = case r of
        Right (Var id) -> appExtender
        Right (Abs id rhs) -> absExtender
        Right (App lhs rhs) -> appExtender
        Right (AppPC lhs rhs) -> appExtender
        Left (AppC lhs rhs) -> appExtender
        Left (VarC id) -> appExtender
        Left (AbsC id rhs) -> appExtender
  if null rest then Extender result (\x y -> error "unhandled blub") else extender result

noopExtender :: Either CompletedParseResult IntermediateParseResult -> Extender
noopExtender (Right i) = Extender (Intermediate i) (\x y -> error "can not extend further")
noopExtender (Left i) = Extender (Completed i) (\x y -> error "can not extend further")

getFirstElement :: [TokenOrScope] -> Extender
getFirstElement x | trace ("getFirstElement: " ++ show x) False = undefined
getFirstElement ((Scope inner) : tokens) = do
  let scopeContent = toCompleted' $ unpackClose $ parse' $ Unhandled inner
  let extendable = Extendable (Closed (Left scopeContent) tokens)
  let traced = trace ("getFirstTrace: " ++ show extendable) extendable
  appExtender traced
getFirstElement (T Lambda : (T (VarUseOrBind id)) : T Dot : Scope s : tokens) = do
  let body = toCompleted' $ unpackClose $ parse' $ Unhandled s
  absExtender (Extendable (E (Abs id (Just $ Left body)) tokens))
getFirstElement (T Lambda : (T (VarUseOrBind id)) : T Dot : tokens) = absExtender (Extendable (E (Abs id Nothing) tokens))
getFirstElement (T (VarUseOrBind id) : tokens) = appExtender (Extendable (E (Var id) tokens))
getFirstElement val = error $ "Unhandled (possibly invalid) sequence: " ++ show val

unpackClose :: ParseResult -> Either CompletedParseResult IntermediateParseResult
unpackClose (Intermediate el) = Right el
unpackClose (Extendable (E el [])) = Right el
unpackClose (Extendable (Closed el [])) = el
unpackClose (Completed el) = Left el
unpackClose val = error $ "unexpected usage: " ++ show val

appExtender :: ParseResult -> Extender
appExtender (Unhandled tks) = getFirstElement tks
appExtender (Extendable (Closed (Left val) [])) = Extender (Completed val) (\x y -> error "can't extend finished")
appExtender (Extendable (Closed (Left val) tks)) = do
  let f newRhs tks = Extendable (Closed (Right $ AppPC val newRhs) tks)
  Extender (Extendable (Closed (Left val) tks)) f
appExtender (Extendable (Closed (Right (AppPC lhs rhs)) ts)) = do
  let appF ecp = case ecp of
        (Left compl) -> AppPC compl
        (Right part) -> App part
  let f newRhs tks = Extendable (Closed (Right $ AppPC lhs (Right $ appF rhs newRhs)) tks)
  Extender (Extendable (Closed (Right $ AppPC lhs rhs) ts)) f
appExtender (Extendable (E orig ts)) = do
  let f rhs tks = Extendable (E (App orig rhs) tks)
  Extender (Extendable (E orig ts)) f
-- appExtender (Extendable (Closed orig ts)) = do
--   let f new tks = Extendable (E (App orig new) tks)
--   Extender (Extendable (Closed orig ts)) f
appExtender val = error $ "unexpected app extend" ++ show val

absExtender :: ParseResult -> Extender
absExtender (Extendable (E orig@(Abs id body) tks)) = do
  let extendBody prev append = case prev of
        Nothing -> Just append
        Just body -> case body of
          (Left completed) -> Just $ Right $ AppPC completed append
          (Right partial) -> Just $ Right $ App partial body
  let f rhs tks = Extendable (E (Abs id (extendBody body rhs)) tks)
  Extender (Extendable (E orig tks)) f
absExtender val = error $ "unexpected abs extend: " ++ show val

-- >>> unpack $ getFirstElement $ generalize [Lambda, VarUseOrBind "a", Dot, VarUseOrBind "a"]
-- Intermediate (Abs "a" Nothing)

-- scope

getScopes :: [TokenOrScope] -> [TokenOrScope]
getScopes tokens = ensureFullResult $ getScope $ Empty ([T OpenParens] ++ tokens ++ [T ClosingParens])

getScope :: ScopeAggregator -> ScopeAggregator
getScope aggr
  | r@(Result {}) <- aggr = r
  | Empty tks@(T OpenParens : tokens) <- aggr = getScope $ Aggregating [] tokens 1
  | Empty tokens <- aggr = getScope $ Aggregating [] tokens 0
  | Aggregating els [] 0 <- aggr = Result els []
  | Aggregating els (T ClosingParens : tokens) depth <- aggr =
    if depth > 1
      then getScope $ Aggregating els tokens (depth - 1)
      else
        if depth == 1
          then Result els tokens
          else error "parenthesis mismatch"
  | Aggregating els [next] depth <- aggr = if depth == 0 then Result (els ++ [next]) [] else error "parenthesis mismatch"
  | Aggregating els tks@(T OpenParens : tokens) depth <- aggr = getScope $ Aggregating els tokens (depth + 1)
  | Aggregating els (next : tokens) depth <- aggr = getScope $ Aggregating (els ++ [next]) tokens depth
  | otherwise = error $ "unhandled scope take: " ++ show aggr

-- manual scope grabber test

-- >>> getScopes $ [T OpenParens, T OpenParens, T (VarUseOrBind "a"), T ClosingParens, T ClosingParens]
-- [T (VarUseOrBind "a")]

-- >>> getScopes $ generalize $ flatMapTokens $ Tokenizer.tokenize "(a(b)c)"
-- [T (VarUseOrBind "a"),T (VarUseOrBind "b"),T (VarUseOrBind "c")]

-- >>> getScopes $ generalize $ flatMapTokens $ Tokenizer.tokenize "a (b c)"
-- [T (VarUseOrBind "a"),T (VarUseOrBind "b"),T (VarUseOrBind "c")]

type BracketDepth = Int

data ScopeAggregator
  = Empty [TokenOrScope]
  | Aggregating [TokenOrScope] [TokenOrScope] BracketDepth
  | Result [TokenOrScope] [TokenOrScope]
  deriving (Show)

ensureFullResult :: ScopeAggregator -> [TokenOrScope]
ensureFullResult (Result done []) = done
ensureFullResult val = error $ "bug, not finished:: " ++ show val

-- helpers

toTerm :: CompletedParseResult -> Term
toTerm (AppC l r) = Lib.App (toTerm l) (toTerm r)
toTerm (AbsC id r) = Lib.Abs id (toTerm r)
toTerm (VarC id) = Lib.Var id

toCompleted :: IntermediateParseResult -> CompletedParseResult
toCompleted (App l r) = AppC (toCompleted l) (toCompleted' r)
toCompleted (Abs id r) = AbsC id (toCompleted' $ fromJust r)
toCompleted (Var id) = VarC id
toCompleted (AppPC l r) = AppC l (toCompleted' r)

toCompleted' :: Either CompletedParseResult IntermediateParseResult -> CompletedParseResult
toCompleted' (Left val) = val
toCompleted' (Right val) = toCompleted val

ensureCompleted :: ParseResult -> CompletedParseResult
ensureCompleted (Completed c) = c
ensureCompleted (Extendable (E el [])) = toCompleted el
ensureCompleted (Extendable (Closed el [])) = toCompleted' el
ensureCompleted (Intermediate i) = toCompleted i
ensureCompleted val = error $ "incomplete parse result: " ++ show val

isCompletedOrError :: Extender -> Bool
-- isCompletedOrError (Extender a b) | trace ("check completed: " ++ show a) False = undefined
isCompletedOrError ex = do
  let check = isCompletedOrError' ex
  let checkt = trace ("isCompl " ++ show (original ex) ++ " ?: " ++ show check) check
  checkt

isCompletedOrError' :: Extender -> Bool
isCompletedOrError' ex = do
  case original ex of
    (Completed c) -> True
    (Extendable (E e [])) -> True
    (Extendable (Closed e [])) -> True
    (Error c) -> True
    (Intermediate r) -> False
    _ -> False
