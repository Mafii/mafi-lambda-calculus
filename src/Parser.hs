module Parser (parse) where

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
  | Abs String (Maybe IntermediateParseResult)
  | App IntermediateParseResult IntermediateParseResult
  deriving (Show)

data CompletedParseResult
  = VarC String
  | AbsC String CompletedParseResult
  | AppC CompletedParseResult CompletedParseResult
  deriving (Show)

data Extendable = E IntermediateParseResult [TokenOrScope]
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
          ensureComplete $
            takeFirstScope $
              generalize $
                flatMapTokens tokens

ensureComplete :: ([TokenOrScope], [TokenOrScope]) -> [TokenOrScope]
ensureComplete (tokens, []) = tokens
ensureComplete (firstScope, unhandled) = do
  let (nextScope, unhandled') = takeFirstScope unhandled
  ensureComplete ([Scope firstScope, Scope nextScope], unhandled')

-- >>> ensureComplete $ takeFirstScope $ generalize $ flatMapTokens $ Tokenizer.tokenize "(a b c) d"
-- [Scope [T (VarUseOrBind "a"),T (VarUseOrBind "b"),T (VarUseOrBind "c")],Scope [T (VarUseOrBind "d")]]

generalize :: [Token] -> [TokenOrScope]
generalize = map T

-- >>> parse $ Tokenizer.tokenize "(a(b)c)"
-- Prelude.undefined

parse' :: ParseResult -> ParseResult
parse' inp | trace ("parse': " ++ show inp) False = undefined
parse' u@(Unhandled tokens) = unpack $ until isCompletedOrError extend (getFirstElement tokens)
parse' p@(Extendable (E extendable unhandled)) = undefined -- unpack $ until isCompletedOrError extend (appExtender p)
parse' c@(Completed {}) = c
parse' e@(Error {}) = e
parse' val = error $ "could not parse: " ++ show val

-- extend

data Extender = Extender
  { original :: ParseResult,
    extendf :: IntermediateParseResult -> [TokenOrScope] -> ParseResult
  }

unpack :: Extender -> ParseResult
unpack = original

extend :: Extender -> Extender
extend input = do
  let (r, rest, f) = case input of
        Extender (Extendable (E interm rest)) f -> (interm, rest, f)
        Extender (Intermediate interm) f -> (interm, [], \x y -> error "unhandled blub")
        Extender a b -> error $ "unhandled" ++ show a
  let next = if null rest then noopExtender r else getFirstElement rest
  let (newEl, newRest) = case next of
        Extender (Extendable (E el rest)) _ -> (el, rest)
        Extender (Intermediate i) _ -> (i, [])
        _ -> error "unhandled extnext"
  let result = f newEl newRest
  let extender = case r of
        Var id -> appExtender
        Abs id rhs -> absExtender
        App lhs rhs -> appExtender
  if null rest then Extender result (\x y -> error "unhandled blub") else extender result

noopExtender :: IntermediateParseResult -> Extender
noopExtender i = Extender (Intermediate i) (\x y -> error "can not extend further")

getFirstElement :: [TokenOrScope] -> Extender
getFirstElement x | trace ("getFirstElement: " ++ show x) False = undefined
getFirstElement ((Scope inner) : tokens) = appExtender (parse' $ Unhandled inner)
getFirstElement (T Lambda : (T (VarUseOrBind id)) : T Dot : tokens) = absExtender (Extendable (E (Abs id Nothing) tokens))
getFirstElement (T (VarUseOrBind id) : tokens) = appExtender (Extendable (E (Var id) tokens))
getFirstElement val = error $ "Unhandled (possibly invalid) sequence: " ++ show val

appExtender :: ParseResult -> Extender
appExtender (Extendable (E orig ts)) = do
  let f rhs tks = Extendable (E (App orig rhs) tks)
  Extender (Extendable (E orig ts)) f
appExtender _ = error "unexpected app extend"

absExtender :: ParseResult -> Extender
absExtender (Extendable (E orig@(Abs id body) tks)) = do
  let extendBody prev append = case prev of
        Nothing -> append
        Just body -> App body append
  let f rhs tks = Extendable (E (Abs id (Just $ extendBody body rhs)) tks)
  Extender (Extendable (E orig tks)) f
absExtender _ = error "unexpected abs extend"

-- >>> unpack $ getFirstElement $ generalize [Lambda, VarUseOrBind "a", Dot, VarUseOrBind "a"]
-- Intermediate (Abs "a" Nothing)

-- scope

takeFirstScope :: [TokenOrScope] -> ([TokenOrScope], [TokenOrScope])
takeFirstScope tokens = ensureResult $ takeFirstScope' $ Empty tokens

takeFirstScope' :: ScopeAggregator -> ScopeAggregator
takeFirstScope' aggr
  | r@(Result {}) <- aggr = r
  | Empty (T OpenParens : tokens) <- aggr = takeFirstScope' $ Aggregating [] tokens 1
  | Empty tokens <- aggr = takeFirstScope' $ Aggregating [] tokens 0
  | Aggregating els (T ClosingParens : tokens) depth <- aggr =
    if depth > 1
      then takeFirstScope' $ Aggregating els tokens (depth - 1)
      else
        if depth == 1
          then Result els tokens
          else error "parenthesis mismatch"
  | Aggregating els [next] depth <- aggr = if depth == 0 then Result (els ++ [next]) [] else error "parenthesis mismatch"
  | Aggregating els (T OpenParens : tokens) depth <- aggr = do
    let (scope, rest) = takeFirstScope (T OpenParens : tokens)
    takeFirstScope' $ Aggregating (els ++ scope) rest depth
  | Aggregating els (next : tokens) depth <- aggr = takeFirstScope' $ Aggregating (els ++ [next]) tokens depth
  | otherwise = error $ "unhandled scope take: " ++ show aggr

-- manual scope grabber test

-- >>> takeFirstScope' $ Empty [OpenParens, OpenParens, (VarUseOrBind "a"), ClosingParens, ClosingParens]
-- Result [Right (Scope [Left (VarUseOrBind "a")])] []

-- >>> takeFirstScope' $ Empty $ flatMapTokens $ Tokenizer.tokenize "(a(b)c)"
-- Result [Left (VarUseOrBind "a"),Right (Scope [Left (VarUseOrBind "b")]),Left (VarUseOrBind "c")] []

type BracketDepth = Int

data ScopeAggregator
  = Empty [TokenOrScope]
  | Aggregating [TokenOrScope] [TokenOrScope] BracketDepth
  | Result [TokenOrScope] [TokenOrScope]
  deriving (Show)

ensureResult :: ScopeAggregator -> ([TokenOrScope], [TokenOrScope])
ensureResult (Result scope rest) = (scope, rest)
ensureResult val = error $ "bug, not finished:: " ++ show val

-- helpers

toTerm :: CompletedParseResult -> Term
toTerm (AppC l r) = Lib.App (toTerm l) (toTerm r)
toTerm (AbsC id r) = Lib.Abs id (toTerm r)
toTerm (VarC id) = Lib.Var id

toCompleted :: IntermediateParseResult -> CompletedParseResult
toCompleted (App l r) = AppC (toCompleted l) (toCompleted r)
toCompleted (Abs id r) = AbsC id (toCompleted $ fromJust r)
toCompleted (Var id) = VarC id

ensureCompleted :: ParseResult -> CompletedParseResult
ensureCompleted (Completed c) = c
ensureCompleted (Extendable (E el [])) = toCompleted el
ensureCompleted (Intermediate i) = toCompleted i
ensureCompleted val = error $ "incomplete parse result: " ++ show val

isCompletedOrError :: Extender -> Bool
isCompletedOrError (Extender a b) | trace ("check completed: " ++ show a) False = undefined
isCompletedOrError ex
  | (Completed c) <- original ex = True
  | (Extendable (E e [])) <- original ex = True
  | (Error c) <- original ex = True
  | (Intermediate r) <- original ex = False
  | otherwise = False
