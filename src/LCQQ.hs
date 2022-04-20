{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE DataKinds #-}

-- Inspired by https://github.com/tweag/HaskellR/blob/ace283d47a89d680d03182461f4dba98da2ee042/inline-r/src/Language/R/QQ.hs
module LCQQ (λ, lambda) where

import Data.Char (isSpace)
import Language.Haskell.TH (Exp (VarE), Q)
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified Language.Haskell.TH.Syntax as TH
import Lib (Term (Abs, App, Var), Id)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Data.Maybe

λ :: QuasiQuoter
λ = QuasiQuoter
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
parse' text = fromJust $ unwrap (interpret (tokenize text) NoTokens) NoTokens -- unwrap . readToEnd . interpret . tokenize

readToEnd :: (AbortReason -> t) -> t
readToEnd f = f NoTokens

unwrap :: GroupingResult -> AbortReason -> Maybe Term
unwrap (Grouping r t ts) allowedReason = if allowedReason == r then t else error $ "Invalid reason: \n  " ++ show r ++ "\n  Expected:" ++ show allowedReason 

tokenize :: String -> [Token]
tokenize text = tokenize' text []

tokenize' :: String -> [Token] -> [Token]
tokenize' "" xs = xs
-- character λ does not work, encoded variant must be used (but it works in (probably unicode that's why) strings)
-- if weird errors happen, add setLocaleEncoding utf8 to your code ahead of running this tokenizer
tokenize' ('\955': restOfString) tokens = tokenize' restOfString (tokens ++ [LambdaCharacter])
tokenize' ('l':'a':'m':'b':'d':'a': restOfString) tokens = tokenize' restOfString (tokens ++ [LambdaWord])
tokenize' ('(': restOfString) tokens = tokenize' restOfString (tokens ++ [OpeningParenthesis])
tokenize' (')': restOfString) tokens = tokenize' restOfString (tokens ++ [ClosingParenthesis])
tokenize' ('.': restOfString) tokens = tokenize' restOfString (tokens ++ [FunctionAbstractionDot])
tokenize' (' ': restOfString) tokens = tokenize' restOfString (tokens ++ [Space])
tokenize' ('\n': restOfString) tokens = tokenize' restOfString (tokens ++ [Newline])
tokenize' (s:ss) [] = tokenize' ss [VariableUsageOrBinding [s]]
tokenize' (s:ss) tokens = tokenize' ss (if isVariable (last tokens)
  then init tokens ++ [appendCharToTokenText (last tokens) s]
  else tokens ++ [VariableUsageOrBinding [s]])

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
data TokenOrPartialAst = Token Token | PartialAst PartialAst

data PartialAst = Empty | Expression [TokenOrPartialAst] | Expressions [PartialAst] | FreeVariableDeclaration String | VariableUsage String

instance Show PartialAst where
  show Empty = "<empty ast>"
  show (Expression tokens) = show tokens
  show (Expressions exps) = show exps
  show (VariableUsage id) = " " ++ id ++ " "
  show (FreeVariableDeclaration id) = "λ." ++ id

instance Show TokenOrPartialAst where
  show (Token token) = show token
  show (PartialAst ast) = show ast

data GroupingResult = Grouping AbortReason (Maybe Term) [Token]
  deriving (Show)
data AbortReason = NoTokens | Parentesis | Dot
  deriving (Show, Eq)

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
interpret _ _ = error "unhandled"

foldRest :: GroupingResult -> AbortReason -> GroupingResult
foldRest (Grouping r t ts) expectedReason
  | Just term <- t = do
    let result = foldRest (interpret ts r) expectedReason
    case unwrap result expectedReason of 
      Just val -> Grouping expectedReason (Just $ App term val) (getRestFromGrouping result)
      Nothing -> Grouping expectedReason (Just term) (getRestFromGrouping result)
  | otherwise = foldRest (interpret ts r) expectedReason

ensureReasonOrThrow :: GroupingResult -> AbortReason -> GroupingResult
ensureReasonOrThrow (Grouping r t ts) expected = if r == expected 
  then Grouping r t ts
  else error $ "Aborted syntax parsing due to invalid syntax.  \n  Hint: Did you close all your parenthesis correctly?" ++ "\n  Expected abort reason: " ++ show expected ++ "\n  Actual abort reason" ++ show r

getParanethesisContent :: [Token] -> GroupingResult
getParanethesisContent tokens = interpret tokens Parentesis

append :: PartialAst -> TokenOrPartialAst -> PartialAst
append (Expression exp) token = Expression (exp ++ [token])
append _ _ = error "Can only append token to expression"

getFunctionAbstractionContent :: [Token] -> AbortReason -> Maybe GroupingResult
getFunctionAbstractionContent tokens outerAbortReason = 
  do let parsed = interpret tokens Dot
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
getBodyOrNothing (Grouping r (Just t) ts) outerAbortReason = Just $ ensureReasonOrThrow (interpret ts outerAbortReason) outerAbortReason
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
-- [lambda,<space>,<var a>,<space>,<dot>,<space>,<var a>]
