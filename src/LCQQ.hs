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

parse' :: String -> Term
parse' = undefined

-- parse' text = fromJust $ unwrap (interpret' (tokenize text)) NoTokens -- unwrap . readToEnd . interpret . tokenize

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

data Ast = Empty | Application Ast Ast | Abstraction String Ast | Variable String
  deriving (Eq)

instance Show Ast where
  show Empty = "<empty>"
  show (Variable id) = id
  show (Application ast ast') = "(" ++ show ast ++ " " ++ show ast' ++ ")"
  show (Abstraction id ast) = "(λ" ++ id ++ "." ++ show ast ++ ")"

data AbortReason = NoTokens | Parenthesis | Dot | MatchOnlyOneElement
  deriving (Show, Eq)

type TakeUntil = AbortReason

data ParseResult = Success Ast [Token] | Abort AbortReason | OuterAbort AbortReason [Token]

instance Show ParseResult where
  show (Success ast []) = show ast
  show (Success ast tokens) = "<Success: (" ++ show ast ++ ") - Unhandled: " ++ show tokens ++ ">"
  show (Abort r) = "Error: " ++ show r
  show (OuterAbort r tokens) = "Outer abort Error: " ++ show r ++ " - " ++ show tokens

-- TODO: handle multiple top level statements
-- Workaround: Add parenthesis atm
-- Maybe this is fine as permanent solution
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
parse''' [] until = if until == NoTokens then Success Empty [] else Abort NoTokens
parse''' (OpeningParenthesis : tokens) until = extendResultUntil (parse''' tokens Parenthesis) until
parse''' (ClosingParenthesis : tokens) until = if until == Parenthesis then Success Empty tokens else Abort Parenthesis
parse''' (VariableUsageOrBinding id : tokens) until = extendResultUntil (Success (Variable id) tokens) until
parse''' (LambdaCharacter : VariableUsageOrBinding id : FunctionAbstractionDot : tokens) until = createAbstraction id (extendResultUntil (parse''' tokens until) until)
parse''' ts r = error $ "parse error: unhandled case: " ++ show ts ++ show r

createAbstraction :: String -> ParseResult -> ParseResult
createAbstraction id (Success ast tokens) = Success (Abstraction id ast) tokens
createAbstraction _ e@(Abort _) = e
createAbstraction _ e@(OuterAbort _ _) = e

resultToTerm :: ParseResult -> Term
resultToTerm = undefined

extendResultUntil :: ParseResult -> TakeUntil -> ParseResult
extendResultUntil s@(Success ast []) until = s
extendResultUntil s@(Success ast tokens) MatchOnlyOneElement = s
extendResultUntil s@(Success ast tokens) until = createApplicationWithResultExtension s until s
extendResultUntil e@(Abort r) until = if r == until then Success Empty [] else e
extendResultUntil e@(OuterAbort r tokens) until = if r == until then Success Empty tokens else e

-- ensures that (a b c) is interpreted as ((a b) c)
-- this method may only call parse''' with MatchOnlyOneElement or it introduces a runtime bug
createApplicationWithResultExtension :: ParseResult -> TakeUntil -> ParseResult -> ParseResult
createApplicationWithResultExtension s@(Success ast tokens) MatchOnlyOneElement _ = error "undefined behaviour, bug in the parser"
createApplicationWithResultExtension s@(Success ast []) until _ = s
createApplicationWithResultExtension s@(Success (Application _ Empty) tokens) _ _ = s
createApplicationWithResultExtension s@(Success ast tokens) until _ =
  createApplicationWithResultExtension (createApplication s (parse''' tokens MatchOnlyOneElement)) until s
createApplicationWithResultExtension e@(OuterAbort r tokens) until _
  | r == until = OuterAbort r tokens
  | otherwise = e
createApplicationWithResultExtension e@(Abort r) until previous@(Success ast tokens)
  | r == until = OuterAbort r tokens
  | otherwise = e
-- createApplicationWithResultExtension e@(Abort r) until previous@(Success ast tokens) = if r == until then Success Empty tokens else e -- Success Empty tokens else e
createApplicationWithResultExtension _ _ _ = error "unhandled parser case"

-- unhandled tokens of lhs are ignored by design as they are used to create rhs and the rest of that is inside rhs
-- TODO: these operations could be simplified by using a custom result monad that would stay error on projection
createApplication :: ParseResult -> ParseResult -> ParseResult
createApplication lhs@(Success ast _) (Success Empty tokens) = Success ast tokens -- Application of any <ast> <- <empty> equals <ast>
createApplication lhs@(Success ast _) rhs@(Success ast' tokens) = Success (Application ast ast') tokens
createApplication lhs@(Success ast tokens) rhs@(Abort reason) = rhs
createApplication _ _ = undefined

-- >>> tokenize "(lambda a. lambda b . (b c)) 5"
-- >>> parse''' (wrapIntoParenthesis $ filterWhitespace $ replaceLambdaWordWithCharacter it) NoTokens
-- [(,lambda,<space>,<var a>,<dot>,<space>,lambda,<space>,<var b>,<space>,<dot>,<space>,(,<var b>,<space>,<var c>,),),<space>,<var 5>]
-- <Success: (((λa.((λb.((b c) <empty>)) <empty>)) <empty>))Unhandled: [),),<var 5>,)]>

-- Bug: (toplevel (sub)) (toplevel') becomes (toplevel (sub toplevel')) which is wrong

-- >>> tokenize "(top) (top') (top''')"
-- >>> parse''' (wrapIntoParenthesis $ filterWhitespace $ replaceLambdaWordWithCharacter it) NoTokens
-- [(,<var top>,),<space>,(,<var top'>,),<space>,(,<var top'''>,)]
-- <Success: ((top <empty>))Unhandled: [),(,<var top'>,),(,<var top'''>,),)]>

-- foldr instead of foldl like behaviour is the bug

-- >>> tokenize "lambda a. a b c 5"
-- >>> parse''' (filterWhitespace $ replaceLambdaWordWithCharacter it) NoTokens
-- [lambda,<space>,<var a>,<dot>,<space>,<var a>,<space>,<var b>,<space>,<var c>,<space>,<var 5>]
-- (λa.(((a b) c) 5))
