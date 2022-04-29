{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser (parse) where

import qualified Data.Foldable
import Debug.Trace (trace)
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
parseUntilDone val | trace "parseUntilDone" False = undefined
parseUntilDone val@(Unhandled until tokens) = parseUntilDone (parse' val)
parseUntilDone expandable@(Expandable result ScopeFinished []) = result
parseUntilDone expandable@(Expandable {}) = parseUntilDone (parse' expandable)
parseUntilDone abs@(Abs id rhs) = abs
parseUntilDone var@(Var {}) = var
parseUntilDone app@(App lhs rhs) = parseUntilDone $ case rhs of
  Abs id ex@(Expandable {}) -> App lhs (Abs id (parse' ex))
  App lhs' ex@(Expandable {}) -> App lhs (App lhs' (parse' ex))
  Var {} -> app
  ex@(Expandable {}) -> App lhs (parse' ex)
  Aborted result ScopeFinished [] -> result
  Aborted result ScopeFinished tokens -> App app (parse' (Unhandled ScopeFinished tokens))
  el -> error $ "unhandled expansion " ++ show el
parseUntilDone aborted@(Aborted result until []) = if until == ScopeFinished then result else error $ "missing closing parens" ++ show until ++ show result
parseUntilDone aborted@(Aborted result r tokens)
  | trace ("parseUntilDone aborted scope finished: " ++ show result ++ show tokens) False = undefined
  | otherwise = do
    let test = parse' $ Unhandled r tokens
    let tracedTest = trace ("test: -> " ++ show test) test
    let rhs = parseUntilDone tracedTest
    let tracedRhs = trace ("cont: ->" ++ show rhs) rhs
    App result tracedRhs

parse' :: IntermediateParseResult -> IntermediateParseResult
parse' val | (trace $ "parse' ->" ++ show val) False = undefined
parse' (Aborted result until (ClosingParens : tokens)) = closeScope result until tokens
parse' (Expandable result until (ClosingParens : tokens)) = closeScope result until tokens
parse' (Unhandled until (ClosingParens : tokens)) = closeScope (Unhandled ScopeFinished tokens) until tokens
parse' (Aborted result until (next : tokens)) = createExpandable next (App result) tokens until
parse' (Expandable result expandUntil (next : tokens)) = createExpandable next (createExpandFactory result) tokens expandUntil
parse' (Unhandled until@(Parenthesis n) ((VarUseOrBind id) : ClosingParens : tokens)) = closeScope (Var id) until tokens
-- parse' (Unhandled until@(Parenthesis n) (OpenParens : (VarUseOrBind id) : tokens)) = undefined
parse' (Unhandled until (OpenParens : tokens)) = parse' (Unhandled (openScope until) tokens)
parse' (Unhandled until (Lambda : (VarUseOrBind id) : Dot : next : tokens)) = createExpandable next (Abs id) tokens until
parse' (Unhandled until ((VarUseOrBind id) : next : tokens)) = createExpandable next (createExpandFactory (Var id)) tokens until
parse' (Unhandled ScopeFinished [VarUseOrBind id]) = Var id
parse' val = error $ "unhandled case" ++ show val

createExpandable :: Token -> (IntermediateParseResult -> IntermediateParseResult) -> [Token] -> AbortReason -> IntermediateParseResult
createExpandable a b c d | (trace $ "createExpandable " ++ show a ++ " -> " ++ show c) False = undefined
createExpandable OpenParens factory tokens until = handleExpandableLookForward (parse' $ Unhandled until (OpenParens : tokens)) factory until
createExpandable Lambda factory tokens until = handleExpandableLookForward (parse' (Unhandled until (Lambda : tokens))) factory until
createExpandable (VarUseOrBind id) factory tokens until = Expandable (factory $ Var id) until tokens
createExpandable tk f tks until = error $ "invalid syntax " ++ show tk ++ " " ++ show tks ++ " " ++ show until

handleExpandableLookForward :: IntermediateParseResult -> (IntermediateParseResult -> IntermediateParseResult) -> AbortReason -> IntermediateParseResult
handleExpandableLookForward nextElement factory until = do
  let creator el = Expandable (factory el)
  case nextElement of
    -- a@(Aborted result r@(Parenthesis n) (ClosingParens : tokens)) -> closeScope result r tokens
    -- a@(Aborted result r@(Parenthesis n) tokens) -> Aborted result r tokens
    a@(Aborted result reason tokens) -> if until == reason then creator result until tokens else error "missing parenthesis probably"
    (Expandable lhs until tokens) -> creator lhs until tokens
    a@(App {}) -> creator a until []
    a@(Abs {}) -> creator a until []
    v@(Var {}) -> creator v until []
    val -> error $ "unhandled forward look case: " ++ show val

createExpandFactory :: IntermediateParseResult -> (IntermediateParseResult -> IntermediateParseResult)
createExpandFactory a | trace "createExpandFactory" False = undefined
createExpandFactory (Abs id rhs) = \new -> Abs id (App rhs new)
createExpandFactory prev@(App lhs rhs) = \new -> App prev new
createExpandFactory var@(Var id) = \new -> App var new
createExpandFactory _ = error "unexpected value factory request"

-- >>> parse $ Tokenizer.tokenize "(lambda a. (a b) 5) 5"
-- (((λ a . (a b)) 5) 5)

-- >>> parse $ Tokenizer.tokenize "lambda a . (lambda b . c) 5"
-- >>> parse $ Tokenizer.tokenize "(a b) (c d)"
-- ((λ a . (λ b . c)) 5)
-- ((a b) (c d))

-- >>> parse $ Tokenizer.tokenize "(a b) (c d e)"
-- ((a b) (Expandable: ((c d) e) should take until: Parenthesis 1 leftovers: [ClosingParens]))

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
