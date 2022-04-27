{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- Inspired by https://github.com/tweag/HaskellR/blob/ace283d47a89d680d03182461f4dba98da2ee042/inline-r/src/Language/R/QQ.hs
module LCQQ (λ, lambda) where

import Language.Haskell.TH (Exp (VarE), Q)
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift)
import qualified Language.Haskell.TH.Syntax as TH
import Lib (Id, Term (Abs, App, Var))
import Tokenizer
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
parse = toExp . parse'

-- >>> :t [|Var "a"|]
-- [|Var "a"|] :: Quote m => m Exp

-- >>> :t [|App (Var "a") (Var "b")|]
-- [|App (Var "a") (Var "b")|] :: Quote m => m Exp

-- >>> [|App (Var "a") (Var "b")|]
-- AppE (AppE (ConE Lib.App) (AppE (ConE Lib.Var) (LitE (StringL "a")))) (AppE (ConE Lib.Var) (LitE (StringL "b")))

-- >>> :t [|Abs "a" (Var "a")|]
-- [|Abs "a" (Var "a")|] :: Quote m => m Exp

toExp :: TH.Quote m => Term -> m Exp
toExp (Var id) = [|Var id|]
toExp (Abs id term) = do
  conE <- [|Abs id|]
  exp <- toExp term
  return $ TH.AppE conE exp
toExp (App term term') = do
  conE <- [|App|]
  exp <- toExp term
  exp' <- toExp term'
  return $ TH.AppE (TH.AppE conE exp) exp'

-- >>> toExp (parse' "lambda a. a b")
-- AppE (AppE (ConE Lib.Abs) (ListE [LitE (CharL 'a')])) (AppE (AppE (ConE Lib.App) (AppE (ConE Lib.Var) (ListE [LitE (CharL 'a')]))) (AppE (ConE Lib.Var) (ListE [LitE (CharL 'b')])))

parse' :: String -> Term
parse' = parse'' . tokenize

-- BNF syntax (Variable, Function Abstraction, Application):
-- <λexp> ::= <var>
--          | λ<var> . <λexp>
--          | ( <λexp> <λexp> )

data Ast
  = Empty
  | Application Ast Ast
  | Abstraction String Ast
  | Variable String
  deriving (Eq, Show)

-- instance Show Ast where
--   show Empty = "<empty>"
--   show (Variable id) = id
--   show (Application ast ast') = "(" ++ show ast ++ " " ++ show ast' ++ ")"
--   show (Abstraction id ast) = "(λ" ++ id ++ "." ++ show ast ++ ")"

data AbortReason
  = NoTokens
  | Parenthesis
  | Dot
  | MatchOnlyOneElement AbortReason
  deriving (Show, Eq)

type TakeUntil = AbortReason

data ParseResult
  = Success Ast [Token]
  | Abort AbortReason [Token]

instance Show ParseResult where
  show (Success ast []) = show ast
  show (Success ast tokens) = "<Success: (" ++ show ast ++ ") - Unhandled: " ++ show tokens ++ ">"
  show (Abort r ts) = "Error: " ++ show r ++ "- Tokens left over: " ++ show ts

-- TODO: handle multiple top level statements
-- Workaround: Add parenthesis atm
-- This is likely fine as permanent solution
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
parse''' [] until = if until == NoTokens then Success Empty [] else Abort NoTokens []
parse''' (OpeningParenthesis : tokens) _ = parseParenthesisContent tokens
parse''' (ClosingParenthesis : tokens) until = if until == Parenthesis then Success Empty tokens else Abort Parenthesis tokens
parse''' (VariableUsageOrBinding id : tokens) until = extendResultUntil (Success (Variable id) tokens) until
parse''' (LambdaCharacter : VariableUsageOrBinding id : FunctionAbstractionDot : tokens) until = parseFunctionAbstraction id tokens until
parse''' ts r = error $ "parse error: unhandled case: " ++ show ts ++ show r

parseParenthesisContent :: [Token] -> ParseResult
parseParenthesisContent tokens = extendResultUntil (parse''' tokens (MatchOnlyOneElement Parenthesis)) Parenthesis

parseFunctionAbstraction :: String -> [Token] -> TakeUntil -> ParseResult
-- if the caller expects us to match only one element, we still need to take as much as we can as all right of the
-- abstraction belongs to the abstraction
parseFunctionAbstraction id tokens (MatchOnlyOneElement outerReason) = parseFunctionAbstraction id tokens outerReason
parseFunctionAbstraction id tokens until = createAbstraction id tokens until

-- BUG
-- >>> parse' "(lambda a. a lambda b. a b) 5"
-- (λa. a (λb. a b) 5)

createAbstraction :: String -> [Token] -> TakeUntil -> ParseResult
createAbstraction id tokens until = do
  let (ast, tokens') = case parse''' tokens (MatchOnlyOneElement until) of
        (Success ast tokens) -> (ast, tokens)
        (Abort r l) -> error "abort"
  let abst = Success (Abstraction id ast) tokens'
  createApplicationWithResultExtension abst until abst

extendResultUntil :: ParseResult -> TakeUntil -> ParseResult
extendResultUntil s@(Success ast []) until = s
extendResultUntil s@(Success ast tokens) (MatchOnlyOneElement _) = s
-- error "needs fix: maybe pass parse''' tokens MatchOnlyOneElement or do somehting else" --(Success Empty tokens)
extendResultUntil s@(Success ast tokens) until = createApplicationWithResultExtension s until s
extendResultUntil e@(Abort _ _) until = error "undefined case"

-- ensures that (a b c) is interpreted as ((a b) c)
-- this method may only call parse''' with MatchOnlyOneElement or it introduces a runtime bug
createApplicationWithResultExtension :: ParseResult -> TakeUntil -> ParseResult -> ParseResult
createApplicationWithResultExtension s@(Success ast tokens) (MatchOnlyOneElement _) _ = error "undefined behaviour, bug in the parser"
createApplicationWithResultExtension s@(Success abs@(Abstraction id term) tokens) until _ = do
  let nextTerm = parse''' tokens (MatchOnlyOneElement until)
  let (tokens, newTerm) = case createApplicationWithResultExtension (createApplication (Success term tokens) nextTerm) until s of
        Success ast tokens' -> (tokens', ast)
        Abort r tokens' -> (if r == until then (tokens', term) else error "invalid parser case")
  Success (Abstraction id newTerm) tokens
createApplicationWithResultExtension s@(Success ast []) until _ = s
createApplicationWithResultExtension s@(Success (Application _ Empty) tokens) _ _ = s
createApplicationWithResultExtension s@(Success ast tokens) until _ =
  createApplicationWithResultExtension (createApplication s (parse''' tokens (MatchOnlyOneElement until))) until s
createApplicationWithResultExtension e@(Abort r leftovers) until previous@(Success ast _)
  | r == until = Success ast leftovers
  | otherwise = e
-- createApplicationWithResultExtension e@(Abort r) until previous@(Success ast tokens) = if r == until then Success Empty tokens else e -- Success Empty tokens else e
createApplicationWithResultExtension _ _ _ = error "unhandled parser case"

-- unhandled tokens of lhs are ignored by design as they are used to create rhs and the rest of that is inside rhs
-- TODO: these operations could be simplified by using a custom result monad that would stay error on projection
createApplication :: ParseResult -> ParseResult -> ParseResult
createApplication lhs@(Success ast _) (Success Empty tokens) = Success ast tokens -- Application of any <ast> <- <empty> equals <ast>
createApplication lhs@(Success ast _) rhs@(Success ast' tokens) = Success (Application ast ast') tokens
createApplication lhs@(Success ast tokens) rhs@(Abort reason leftovers) = rhs
createApplication _ _ = error "unhandled parser case"

resultToTerm :: ParseResult -> Term
resultToTerm (Success ast []) = astToTerm ast
resultToTerm (Success ast tokens) = error $ "unhandled tokens left over: " ++ show tokens
resultToTerm (Abort r tokens) = error $ "Aborted in unhandled case: " ++ show r ++ " - left over tokens: " ++ show tokens

astToTerm :: Ast -> Term
astToTerm Empty = error "Empty ast represents nothing, should have been removed during parsing. Probably invalid input or a bug."
astToTerm (Variable id) = Var id
astToTerm (Application lhs rhs) = App (astToTerm lhs) (astToTerm rhs)
astToTerm (Abstraction id body) = Abs id (astToTerm body)

-- testing:

-- >>> parse' "lambda a . a"
-- >>> parse' "(((lambda a . a b)) 5)"
-- (λa. a)
-- (λa. a b) 5

-- bug investigation:

-- >>> tokenize "(lambda a. b lambda d . e) c 5"
-- >>> parse''' (wrapIntoParenthesis $ filterWhitespace $ replaceLambdaWordWithCharacter it) NoTokens
-- [(,lambda,<space>,<var a>,<dot>,<space>,<var b>,<space>,lambda,<space>,<var d>,<space>,<dot>,<space>,<var e>,),<space>,<var c>,<space>,<var 5>]
-- Variable "b"[λ,<var d>,<dot>,<var e>,),<var c>,<var 5>,)]

-- old test runs

test :: [Token] -> ParseResult
test it = parse''' (filterWhitespace $ replaceLambdaWordWithCharacter it) NoTokens

-- >>> tokenize "(lambda a. a b g) 5"
-- >>> test it
-- >>> tokenize "lambda a. a b g"
-- >>> test it
-- [(,lambda,<space>,<var a>,<dot>,<space>,<var a>,<space>,<var b>,<space>,<var g>,),<space>,<var 5>]
-- ((λa.((a b) g)) 5)
-- [lambda,<space>,<var a>,<dot>,<space>,<var a>,<space>,<var b>,<space>,<var g>]
-- (λa.((a b) g))

-- >>> tokenize "lambda a . (a b g)"
-- >>> test it
-- [lambda,<space>,<var a>,<space>,<dot>,<space>,(,<var a>,<space>,<var b>,<space>,<var g>,)]
-- (λa.((a b) g))

-- >>> test $ tokenize "lambda a . a"
-- >>> test $ tokenize "(a b c d)"
-- (λa.a)
-- (((a b) c) d)

-- >>> test $ tokenize "(((a b)))"
-- (a b)

-- >>> test $ tokenize "((lambda a . b) 5)"
-- ((λa.b) 5)

-- >>> test $ tokenize "((lambda a . lambda b . 5) 8)"
-- (λa.((λb.5) 8))
