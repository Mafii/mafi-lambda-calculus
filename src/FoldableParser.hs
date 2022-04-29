{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module FoldableParser where

import qualified Data.Foldable
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

data IntermediateParseResult
  = Var String
  | Abs String IntermediateParseResult
  | App IntermediateParseResult IntermediateParseResult
  | Expandable IntermediateParseResult AbortReason [Token]
  | Aborted IntermediateParseResult AbortReason [Token]
  | Unhandled AbortReason [Token]

parse :: [Tokenizer.Token] -> IntermediateParseResult
parse tokens = parseUntilDone (Unhandled ScopeFinished (flatMapTokens tokens))

parseUntilDone :: IntermediateParseResult -> IntermediateParseResult
parseUntilDone val@(Unhandled until tokens) = parseUntilDone (parse' val)
parseUntilDone expandable@(Expandable result ScopeFinished []) = result
parseUntilDone expandable@(Expandable {}) = parseUntilDone (parse' expandable)
parseUntilDone abs@(Abs {}) = abs
parseUntilDone var@(Var {}) = var
parseUntilDone app@(App {}) = app
parseUntilDone aborted@(Aborted result until (next : tokens)) = parseUntilDone $ parse' aborted -- createExpandable next (App result) tokens until
parseUntilDone aborted@(Aborted result until []) = if until == ScopeFinished then result else error $ "missing closing parens" ++ show until ++ show result

parse' :: IntermediateParseResult -> IntermediateParseResult
parse' (Aborted result until (ClosingParens : tokens)) = closeScope result until tokens
parse' (Expandable result until (ClosingParens : tokens)) = closeScope result until tokens
parse' (Unhandled until (ClosingParens : tokens)) = closeScope (Unhandled ScopeFinished tokens) until tokens
parse' (Aborted result until (next : tokens)) = createExpandable next (App result) tokens until
parse' (Expandable result expandUntil (next : tokens)) = createExpandable next (createExpandFactory result) tokens expandUntil
parse' (Unhandled until@(Parenthesis n) ((VarUseOrBind id) : ClosingParens : tokens)) = closeScope (Var id) until tokens
parse' (Unhandled until (OpenParens : tokens)) = parse' (Unhandled (openScope until) tokens)
parse' (Unhandled until (Lambda : (VarUseOrBind id) : Dot : next : tokens)) = createExpandable next (Abs id) tokens until
parse' (Unhandled until ((VarUseOrBind id) : next : tokens)) = createExpandable next (createExpandFactory (Var id)) tokens until
parse' (Unhandled until [VarUseOrBind id]) = Var id
parse' _ = error "unhandled case"

createExpandable :: Token -> (IntermediateParseResult -> IntermediateParseResult) -> [Token] -> AbortReason -> IntermediateParseResult
createExpandable OpenParens factory tokens until = Expandable (factory $ parse' (Unhandled (openScope until) tokens)) until tokens
createExpandable Lambda factory tokens until = Expandable (factory $ parse' (Unhandled until (Lambda : tokens))) until tokens
createExpandable (VarUseOrBind id) factory tokens until = Expandable (factory $ Var id) until tokens
createExpandable tk f tks until = error $ "invalid syntax " ++ show tk ++ show tks ++ show until

createExpandFactory :: IntermediateParseResult -> (IntermediateParseResult -> IntermediateParseResult)
createExpandFactory (Abs id rhs) = \new -> Abs id (App rhs new)
createExpandFactory prev@(App lhs rhs) = \new -> App prev new
createExpandFactory var@(Var id) = \new -> App var new
createExpandFactory _ = error "unexpected value factory request"

-- >>> parse $ Tokenizer.tokenize "((a) b c)"
-- ((a b) c)

openScopes :: AbortReason -> Int
openScopes (Parenthesis n) = n
openScopes r = 0

instance Show IntermediateParseResult where
  show (Var id) = id
  show (Abs id rhs) = "(λ " ++ id ++ " . " ++ show rhs ++ ")"
  show (App lhs rhs) = "(" ++ show lhs ++ " " ++ show rhs ++ ")"
  show (Expandable lhs until tokens) = "(Expandable: " ++ show lhs ++ " should take until: " ++ show until ++ " leftovers: " ++ show tokens ++ ")"
  show (Aborted lhs until leftovers) = "(Aborted: " ++ show until ++ " " ++ show lhs ++ " leftovers: " ++ show leftovers ++ ")"
  show (Unhandled until tokens) = "(Unhandled: " ++ show until ++ " tokens: " ++ show tokens ++ ")"

closeScope :: IntermediateParseResult -> AbortReason -> [Token] -> IntermediateParseResult
closeScope innerResult expectedReason leftovers = do
  let openScopes' = openScopes expectedReason
  let reason = if openScopes' == 1 then ScopeFinished else Parenthesis (openScopes' - 1)
  if openScopes' > 0
    then Aborted innerResult reason leftovers
    else error "unexpected closing parenthesis"

openScope :: AbortReason -> AbortReason
openScope (Parenthesis n) = Parenthesis (n + 1)
openScope ScopeFinished = Parenthesis 1
