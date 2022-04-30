module Parser (parse) where

import qualified Data.Foldable
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

data AbortReason
  = ScopeFinished
  | Parenthesis Int
  deriving (Show, Eq)

type TokenOrStep = Either Token IntermediateParseResult

data IntermediateParseResult
  = Var String
  | Abs String IntermediateParseResult
  | App CompletedParseResult ParseResult
  | Scope [TokenOrStep]
  deriving (Show)

data CompletedParseResult
  = VarC String
  | AbsC String CompletedParseResult
  | AppC CompletedParseResult CompletedParseResult
  deriving (Show)

data ParseResult
  = Completed CompletedParseResult
  | Partial IntermediateParseResult
  | Unhandled [TokenOrStep]
  | Error String
  deriving (Show)

parse :: [Tokenizer.Token] -> Term
parse tokens = toTerm $ ensureCompleted $ parse' $ Unhandled $ generalize $ flatMapTokens tokens

generalize :: [Token] -> [TokenOrStep]
generalize = map Left

-- >>> parse' $ Unhandled $ generalize $ flatMapTokens $ Tokenizer.tokenize "(a(b)c)"

parse' :: ParseResult -> ParseResult
parse' inp | trace ("parse': " ++ show inp) False = undefined
parse' u@(Unhandled tokens) = until isCompletedOrError parseStep u
parse' p@(Partial partial) = until isCompletedOrError parseStep p
parse' c@(Completed {}) = c
parse' e@(Error {}) = e

parsePartial :: IntermediateParseResult -> ParseResult
parsePartial input
  | Scope [Left Lambda, Left Dot, Left (VarUseOrBind id)] <- input = error "missing body for abstraction"
  | Scope (Left Lambda : Left Dot : Left (VarUseOrBind id) : rest) <- input = Partial $ Abs id (Scope rest)
  | Scope [Left (VarUseOrBind id)] <- input = Completed $ VarC id
  | Scope (Left (VarUseOrBind id) : tokens) <- input = Partial $ App (VarC id) (Unhandled tokens)
  | App completed (Completed c) <- input = Completed $ AppC completed c
  | App completed (Unhandled []) <- input = Completed completed
  | App completed unh@(Unhandled elements) <- input = do
    let (lhs, rest) = case parseStep unh of
          (Partial (App lhs rest)) -> (lhs, rest)
          _ -> error "unhandled"
    let returnValue = Partial $ App (AppC completed lhs) rest
    trace ("hey: " ++ show returnValue) returnValue
  | otherwise = undefined

parseStep :: ParseResult -> ParseResult
-- parseStep (Partial Scope) = undefined
parseStep (Unhandled tokens) = do
  let (scope, rest) = takeFirstScope tokens
  Partial $ App (ensureCompleted $ parse' $ Partial $ Scope scope) (Unhandled rest) -- todo here is a bug
parseStep (Partial (App result (Unhandled []))) = Completed result
parseStep (Partial partial@(App result (Unhandled tokens))) = parsePartial partial
parseStep (Partial p@(Scope els)) = parsePartial p
parseStep val = error $ "parseStep: " ++ show val

takeFirstScope :: [TokenOrStep] -> ([TokenOrStep], [TokenOrStep])
takeFirstScope tokens = ensureResult $ takeFirstScope' $ Empty tokens

takeFirstScope' :: ScopeAggregator -> ScopeAggregator
takeFirstScope' aggr
  | r@(Result {}) <- aggr = r
  | Empty (Left OpenParens : tokens) <- aggr = takeFirstScope' $ Aggregating [] tokens 1
  | Empty tokens <- aggr = takeFirstScope' $ Aggregating [] tokens 0
  | Aggregating els (Left ClosingParens : tokens) depth <- aggr =
    if depth > 1
      then takeFirstScope' $ Aggregating els tokens (depth - 1)
      else
        if depth == 1
          then Result els tokens
          else error "parenthesis mismatch"
  | Aggregating els [next] depth <- aggr = if depth == 0 then Result (els ++ [next]) [] else error "parenthesis mismatch"
  | Aggregating els (Left OpenParens : tokens) depth <- aggr = do
    let (scope, rest) = takeFirstScope (Left OpenParens : tokens)
    takeFirstScope' $ Aggregating (els ++ [Right $ Scope scope]) rest depth
  | Aggregating els (next : tokens) depth <- aggr = takeFirstScope' $ Aggregating (els ++ [next]) tokens depth
  | otherwise = error $ "unhandled scope take: " ++ show aggr

-- manual scope grabber test

-- >>> takeFirstScope' $ Empty [OpenParens, OpenParens, (VarUseOrBind "a"), ClosingParens, ClosingParens]
-- Result [Right (Scope [Left (VarUseOrBind "a")])] []

-- >>> takeFirstScope' $ Empty $ flatMapTokens $ Tokenizer.tokenize "(a(b)c)"
-- Result [Left (VarUseOrBind "a"),Right (Scope [Left (VarUseOrBind "b")]),Left (VarUseOrBind "c")] []

type BracketDepth = Int

data ScopeAggregator
  = Empty [TokenOrStep]
  | Aggregating [TokenOrStep] [TokenOrStep] BracketDepth
  | Result [TokenOrStep] [TokenOrStep]
  deriving (Show)

ensureResult :: ScopeAggregator -> ([TokenOrStep], [TokenOrStep])
ensureResult (Result scope rest) = (scope, rest)
ensureResult val = error $ "bug, not finished:: " ++ show val

-- helpers

toTerm :: CompletedParseResult -> Term
toTerm (AppC l r) = Lib.App (toTerm l) (toTerm r)
toTerm (AbsC id r) = Lib.Abs id (toTerm r)
toTerm (VarC id) = Lib.Var id

ensureCompleted :: ParseResult -> CompletedParseResult
ensureCompleted (Completed c) = c
ensureCompleted val = error $ "incomplete parse result: " ++ show val

isCompletedOrError :: ParseResult -> Bool
-- isCompletedOrError v | trace ("check completed: " ++ show v) False = undefined
isCompletedOrError (Completed c) = True
isCompletedOrError (Error c) = True
isCompletedOrError _ = False
