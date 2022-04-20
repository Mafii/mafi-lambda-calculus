{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- Inspired by https://github.com/tweag/HaskellR/blob/ace283d47a89d680d03182461f4dba98da2ee042/inline-r/src/Language/R/QQ.hs
module LCQQ (λ, lambda) where

import Data.Char (isSpace)
import Data.Maybe (fromJust)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Language.Haskell.TH (Exp (VarE), Q)
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified Language.Haskell.TH.Syntax as TH
import Lib (Id, Term (Abs, App, Var))

λ :: QuasiQuoter
λ =
  QuasiQuoter
    { quoteExp = parse,
      quotePat = undefined "quotePat",
      quoteType = undefined "quoteType",
      quoteDec = undefined "quoteDec"
    }

lambda :: QuasiQuoter
lambda = λ

parse :: String -> Q TH.Exp
parse = undefined

data Token = OpeningParenthesis | ClosingParenthesis | LambdaCharacter | LambdaWord | VariableUsageOrBinding String | FunctionAbstractionDot | Space | Newline
  deriving (Eq)

instance Show Token where
  show OpeningParenthesis = "("
  show ClosingParenthesis = ")"
  show FunctionAbstractionDot = "<dot>"
  show LambdaCharacter = "λ"
  show LambdaWord = "lambda"
  show Space = "<space>"
  show Newline = "<newline>"
  show (VariableUsageOrBinding id) = "<var " ++ id ++ ">"

-- >>> OpeningParenthesis
-- >>> VariableBinding "abc"
-- OpeningParenthesis
-- VariableBinding "abc"

parse' :: String -> Term
parse' = undefined

-- parse' text = fromJust $ unwrap (interpret' (tokenize text)) NoTokens -- unwrap . readToEnd . interpret . tokenize

{-
readToEnd :: (AbortReason -> t) -> t
readToEnd f = f NoTokens

unwrap :: GroupingResult -> AbortReason -> Maybe Term
unwrap (Grouping r t ts) allowedReason = if allowedReason == r then t else t -- error $ "Invalid reason: \n  " ++ show r ++ "\n  Expected:" ++ show allowedReason
-}

tokenize :: String -> [Token]
tokenize text = tokenize' text []

tokenize' :: String -> [Token] -> [Token]
tokenize' "" xs = xs
-- character λ does not work, encoded variant must be used (but it works in (probably unicode that's why) strings)
-- if weird errors happen, add setLocaleEncoding utf8 to your code ahead of running this tokenizer
tokenize' ('\955' : restOfString) tokens = tokenize' restOfString (tokens ++ [LambdaCharacter])
tokenize' ('l' : 'a' : 'm' : 'b' : 'd' : 'a' : restOfString) tokens = tokenize' restOfString (tokens ++ [LambdaWord])
tokenize' ('(' : restOfString) tokens = tokenize' restOfString (tokens ++ [OpeningParenthesis])
tokenize' (')' : restOfString) tokens = tokenize' restOfString (tokens ++ [ClosingParenthesis])
tokenize' ('.' : restOfString) tokens = tokenize' restOfString (tokens ++ [FunctionAbstractionDot])
tokenize' (' ' : restOfString) tokens = tokenize' restOfString (tokens ++ [Space])
tokenize' ('\n' : restOfString) tokens = tokenize' restOfString (tokens ++ [Newline])
tokenize' (s : ss) [] = tokenize' ss [VariableUsageOrBinding [s]]
tokenize' (s : ss) tokens =
  tokenize'
    ss
    ( if isVariable (last tokens)
        then init tokens ++ [appendCharToTokenText (last tokens) s]
        else tokens ++ [VariableUsageOrBinding [s]]
    )

isVariable :: Token -> Bool
isVariable (VariableUsageOrBinding _) = True
isVariable _ = False

appendCharToTokenText :: Token -> Char -> Token
appendCharToTokenText (VariableUsageOrBinding t) c = VariableUsageOrBinding (t ++ [c])
appendCharToTokenText _ _ = error "only defined for variables"

-- >>> setLocaleEncoding utf8
-- >>> tokenize "a"
-- >>> tokenize "λa. a b"
-- >>> tokenize "(λb . (λa. a b)) 5"
-- [<var a>]
-- [λ,<var a>,<dot>,<space>,<var a>,<space>,<var b>]
-- [(,λ,<var b>,<space>,<dot>,<space>,(,λ,<var a>,<dot>,<space>,<var a>,<space>,<var b>,),),<space>,<var 5>]

-- BNF syntax (Variable, Function Abstraction, Application):
-- <λexp> ::= <var>
--          | λ<var> . <λexp>
--          | ( <λexp> <λexp> )

-- (Either Token PartialAst)-like type to manually overwrite show
-- data TokenOrPartialAst = Token Token | PartialAst PartialAst

-- data PartialAst = Empty | Expression [TokenOrPartialAst] | Expressions [PartialAst] | FreeVariableDeclaration String | VariableUsage String

--instance Show PartialAst where
-- show Empty = "<empty ast>"
--  show (Expression tokens) = show tokens
-- show (Expressions exps) = show exps
-- show (VariableUsage id) = " " ++ id ++ " "
-- show (FreeVariableDeclaration id) = "λ." ++ id

--instance Show TokenOrPartialAst where
--show (Token token) = show token
-- show (PartialAst ast) = show ast

data Ast = Empty | Application Ast Ast | Abstraction String Ast | Variable String
  deriving (Eq)

instance Show Ast where
  show Empty = "<empty>"
  show (Variable id) = id
  show (Application ast ast') = "(" ++ show ast ++ " " ++ show ast' ++ ")"
  show (Abstraction id ast) = "λ" ++ id ++ "." ++ show ast

data AbortReason = NoTokens | Parenthesis | Dot
  deriving (Show, Eq)

type TakeUntil = AbortReason

data ParseResult = Success Ast [Token] | Error AbortReason

instance Show ParseResult where
  show (Success ast []) = show ast
  show (Success ast tokens) = "<Success: (" ++ show ast ++ ")" ++ "Unhandled: " ++ show tokens ++ ">"
  show (Error r) = "Error: " ++ show r

-- TODO: handle multiple top level statements
-- Workaround: Add parenthesis atm
parse'' :: [Token] -> Term
parse'' [] = error "No Tokens"
parse'' tokens = resultToTerm $ parse''' (wrapIntoParenthesis $ filterWhitespace $ replaceLambdaWordWithCharacter tokens) NoTokens

filterWhitespace :: [Token] -> [Token]
filterWhitespace = filter (\(t :: Token) -> t /= Space && t /= Newline)

replaceLambdaWordWithCharacter :: [Token] -> [Token]
replaceLambdaWordWithCharacter = map (\(t :: Token) -> if t == LambdaWord then LambdaCharacter else t)

wrapIntoParenthesis :: [Token] -> [Token]
wrapIntoParenthesis ts = OpeningParenthesis : ts ++ [ClosingParenthesis]

-- expects no whitespace tokens, and lambda token over characters
-- TODO: ensure this is by design with types
parse''' :: [Token] -> TakeUntil -> ParseResult
parse''' [] until = if until == NoTokens then Success Empty [] else Error NoTokens
parse''' (OpeningParenthesis : tokens) until = extendResultUntil (parse''' tokens Parenthesis) until
parse''' (ClosingParenthesis : tokens) until = if until == Parenthesis then Success Empty tokens else Error Parenthesis
parse''' (VariableUsageOrBinding id : tokens) until = extendResultUntil (Success (Variable id) tokens) until
parse''' (LambdaCharacter : VariableUsageOrBinding id : FunctionAbstractionDot : tokens) until = createAbstraction id (extendResultUntil (parse''' tokens until) until)
parse''' ts r = error $ "unhandled: " ++ show ts ++ show r

createAbstraction :: String -> ParseResult -> ParseResult
createAbstraction id (Success ast tokens) = Success (Abstraction id ast) tokens
createAbstraction _ e@(Error _) = e

-- >>> tokenize "(a b)"
-- >>> parse''' (wrapIntoParenthesis $ filterWhitespace $ replaceLambdaWordWithCharacter it) NoTokens
-- [(,<var a>,<space>,<var b>,)]
-- ((a (b <empty>)) <empty>)

resultToTerm :: ParseResult -> Term
resultToTerm = undefined

extendResultUntil :: ParseResult -> TakeUntil -> ParseResult
extendResultUntil s@(Success ast []) until = if until == NoTokens then s else Error NoTokens
extendResultUntil s@(Success ast tokens) until = createApplication s (parse''' tokens until)
extendResultUntil e@(Error r) until = e

-- unhandled tokens of lhs are ignored
-- TODO: these operations could be simplified by using a custom result monad that would stay error on projection
createApplication :: ParseResult -> ParseResult -> ParseResult
createApplication lhs@(Success ast _) rhs@(Success ast' tokens) = Success (Application ast ast') tokens
createApplication lhs rhs = error $ "left or right is error: \n  Left: " ++ show lhs ++ "\n  Right:" ++ show rhs

{-
interpret :: [Token] -> AbortReason -> GroupingResult
interpret [] _ = Grouping NoTokens Nothing []
interpret (OpeningParenthesis : tokens) r = foldRest (getParanethesisContent tokens) r
interpret (ClosingParenthesis : tokens) Parentesis = Grouping Parentesis Nothing tokens
interpret (LambdaCharacter : tokens) r = foldRest (fromJust $ getFunctionAbstractionContent tokens r) r
interpret (LambdaWord : tokens) r = foldRest (fromJust $ getFunctionAbstractionContent tokens r) r -- todo: allow lambda as variable name when context is clear (this requires look-ahead and rewind if we fail binding lambda as function marker)
interpret (VariableUsageOrBinding id : tokens) r = foldRest (Grouping r (Just $ Var id) tokens) r
interpret (FunctionAbstractionDot : tokens) Dot = Grouping Dot Nothing tokens
-- ignore whitespace
interpret (Space : tokens) r = interpret tokens r
interpret (Newline : tokens) r = interpret tokens r
-- unexpected cases (e.g. unbalanced parenthesis)
interpret ts r = error $ "unhandled" ++ show ts ++ show r
-}

{-
foldRest :: GroupingResult -> AbortReason -> GroupingResult
foldRest (Grouping r t ts) expectedReason
  | Just term <- t = do
    let result = foldRest (interpret ts r) expectedReason
    case unwrap result expectedReason of
      Just val -> Grouping expectedReason (Just $ App term val) (getRestFromGrouping result)
      Nothing -> Grouping expectedReason (Just term) (getRestFromGrouping result)
  | otherwise = foldRest (interpret ts r) expectedReason
-}
{-
ensureReasonOrThrow :: GroupingResult -> AbortReason -> GroupingResult
ensureReasonOrThrow (Grouping r t ts) expected =
  if r == expected
    then Grouping r t ts
    else error $ "Aborted syntax parsing due to invalid syntax.  \n  Hint: Did you close all your parenthesis correctly?" ++ "\n  Expected abort reason: " ++ show expected ++ "\n  Actual abort reason" ++ show r

getParanethesisContent :: [Token] -> GroupingResult
-- getParanethesisContent tokens = interpret tokens Parentesis
getParanethesisContent tokens = interpret' tokens

append :: PartialAst -> TokenOrPartialAst -> PartialAst
append (Expression exp) token = Expression (exp ++ [token])
append _ _ = error "Can only append token to expression"

getFunctionAbstractionContent :: [Token] -> AbortReason -> Maybe GroupingResult
getFunctionAbstractionContent tokens outerAbortReason =
  do
    -- let parsed = interpret tokens Dot
    let parsed = interpret' tokens
    term <- unwrap parsed Dot
    var <- getVarOrNothing term
    id <- getId var
    body <- getBodyOrNothing parsed outerAbortReason
    let rest = getRestFromGrouping body
    term <- getTermFromBody body
    let abs = Abs id term
    Just $ Grouping outerAbortReason (Just $ Abs id term) rest

getVarOrNothing :: Term -> Maybe Term
getVarOrNothing (Var id) = Just $ Var id
getVarOrNothing _ = Nothing

getBodyOrNothing :: GroupingResult -> AbortReason -> Maybe GroupingResult
getBodyOrNothing (Grouping r (Just t) ts) outerAbortReason = Just $ ensureReasonOrThrow (interpret' ts) outerAbortReason
getBodyOrNothing _ _ = Nothing

getId :: Term -> Maybe Id
getId (Var id) = Just id
getId _ = Nothing

getRestFromGrouping :: GroupingResult -> [Token]
getRestFromGrouping (Grouping _ _ ts) = ts

getTermFromBody :: GroupingResult -> Maybe Term
getTermFromBody (Grouping _ t _) = t

-- >>> tokenize "lambda a . ab"
-- >>> -- interpret it NoTokens
-- [lambda,<space>,<var a>,<space>,<dot>,<space>,<var ab>]

-- >>> tokenize "lambda a . ab"
-- >>> interpret' it
-- [lambda,<space>,<var a>,<space>,<dot>,<space>,<var ab>]
-- Maybe.fromJust: Nothing

-- >>> interpret' $ tokenize "lambda a.a"
-- Maybe.fromJust: Nothing
-}
