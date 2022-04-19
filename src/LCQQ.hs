{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- Inspired by https://github.com/tweag/HaskellR/blob/ace283d47a89d680d03182461f4dba98da2ee042/inline-r/src/Language/R/QQ.hs
module LCQQ (λ, lambda) where

import Data.Char (isSpace)
import Language.Haskell.TH (Exp (VarE), Q)
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified Language.Haskell.TH.Syntax as TH
import Lib (Term (Abs, App, Var))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Data.Either hiding (Show (Either))

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
parse' = interpret . groupExpressions . tokenize

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

-- complete BNF syntax (Variable, Function Abstraction, Application):
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

groupExpressions :: [Token] -> PartialAst
groupExpressions (ClosingParenthesis: _) = error "Unbalanced parantheses, could not parse into lc expression"
groupExpressions [LambdaWord] = error "Invalid function expression"
groupExpressions [LambdaCharacter] = error "Invalid function expression"
groupExpressions (OpeningParenthesis: tokens) = handleRest (getParanethesisContent tokens Empty)
  where
    handleRest (WithoutRest ast) = ast
    handleRest (WithRest ast rest) = Expressions $ ast : [groupExpressions rest]
groupExpressions (LambdaCharacter: tokens) = getFunctionAbstractionContent tokens Empty
groupExpressions (LambdaWord: tokens) = getFunctionAbstractionContent tokens Empty
groupExpressions [VariableUsageOrBinding id] = VariableUsage id
groupExpressions ((VariableUsageOrBinding id): tokens) = Expressions (VariableUsage id : [groupExpressions tokens])
groupExpressions (Space: tokens) = groupExpressions tokens -- we simply ignore whitespace
groupExpressions _ = error "unhandled case, likely invalid syntax"

data GroupingResult = WithRest PartialAst [Token] | WithoutRest PartialAst
  deriving (Show)

getParanethesisContent :: [Token] -> PartialAst -> GroupingResult
getParanethesisContent [] _ = error "Unbalanced parantheses, could not parse into lc expression"
getParanethesisContent [ClosingParenthesis] (Expression tokensAndAsts) = WithoutRest (Expression tokensAndAsts)
getParanethesisContent (ClosingParenthesis: tokens) (Expression tokensAndAsts) = WithRest (Expression tokensAndAsts) tokens
getParanethesisContent (OpeningParenthesis: tokens) (Expression tokensAndAsts) = handleRest getParanethesisContent tokens tokensAndAsts
-- getParanethesisContent (OpeningParenthesis: tokens) (Expression tokensAndAsts) = do
--   let (result, rest) = case getParanethesisContent tokens Empty of (WithRest ast r) -> (ast, r)
--                                                                    (WithoutRest ast) -> (ast, [])
--   getParanethesisContent rest (Expression (tokensAndAsts ++ [PartialAst result]))
getParanethesisContent (token: tokens) Empty = getParanethesisContent tokens (append (Expression []) (Token token))
getParanethesisContent (token: tokens) exp = getParanethesisContent tokens (append exp (Token token))

append :: PartialAst -> TokenOrPartialAst -> PartialAst
append (Expression exp) token = Expression (exp ++ [token])
append _ _ = error "Can only append token to expression"

-- >>> getParanethesisContent (tokenize "lambda a. a (b))") Empty
-- >>> getParanethesisContent (tokenize "a x (b))") Empty
-- WithoutRest ([lambda,<space>,<var a>,<dot>,<space>,<var a>,<space>,([<var b>])])
-- WithoutRest ([<var a>,<space>,<var x>,<space>,([<var b>])])

getFunctionAbstractionContent :: [Token] -> PartialAst -> PartialAst
getFunctionAbstractionContent (FunctionAbstractionDot: tokens) (Expression tokensAndAsts) = Expression (tokensAndAsts ++ [PartialAst (groupExpressions tokens)])
getFunctionAbstractionContent ((VariableUsageOrBinding id): tokens) Empty = getFunctionAbstractionContent tokens (Expression [PartialAst $ FreeVariableDeclaration id])
getFunctionAbstractionContent (Space: tokens) Empty = getFunctionAbstractionContent tokens Empty -- we ignore whitespace here
getFunctionAbstractionContent (Space: tokens) (Expression tokensAndAsts) = getFunctionAbstractionContent tokens (Expression tokensAndAsts) -- we ignore whitespace here
getFunctionAbstractionContent _ _ = error "Error parsing function abstraction. Did you acidentially add multiple parameters? LC only allows for one parameter to its function abstractions. \n  Valid: λ a .<anything> \n  Also Valid: λa.a \n  Invalid: λa b.<anything>"

-- >>> (tokenize "a .(a)")
-- >>> getFunctionAbstractionContent it Empty
-- [<var a>,<space>,<dot>,(,<var a>,)]
-- ([λ.a,([<var a>])])


handleRest :: ([Token] -> PartialAst -> GroupingResult) -> [Token] -> [TokenOrPartialAst] -> GroupingResult
handleRest handler tokens tokensSoFar = do
  let (result, rest) = toTuple $ handler tokens Empty
  handler rest (Expression (tokensSoFar ++ [PartialAst result]))
  where toTuple result = case result of (WithRest ast r) -> (ast, r)
                                        (WithoutRest ast) -> (ast, [])

-- >>> setLocaleEncoding utf8 
-- >>> (tokenize "λa. a (b))")
-- [λ,<var a>,<dot>,<space>,<var a>,<space>,(,<var b>,),)]

interpret :: PartialAst -> Term
interpret Empty = error "Empty token list can not be interpreted"
interpret (Expression [PartialAst (FreeVariableDeclaration id), PartialAst body]) = Abs id (interpret body)
interpret (Expression tokensAndAsts) = error "not sure if this path is ever used so it is disabled temporarily" -- interpret' tokensAndAsts
interpret (Expressions [lhs,rhs]) = App (interpret lhs) (interpret rhs)
interpret (Expressions xs) = error $ "Maximum of 2 expressions can be at the same level (e.g. application of rhs to lhs), but there are" ++ show (length xs)
interpret (VariableUsage id) = Var id
interpret (FreeVariableDeclaration id) = error $ "Can not have anonymous function declaration without body" ++ "\n  hint: The Variable declaration is similar to " ++ show (FreeVariableDeclaration id)

interpret' :: [TokenOrPartialAst] -> Term
interpret' [PartialAst (VariableUsage id)] = Var id
interpret' (PartialAst (VariableUsage id) : rest) = App (Var id) (interpret' rest)
interpret' _ = error "unexpected token or partial ast for application interpretation"

-- >>> parse' "a b c"
-- a b c

-- >>> parse' "λa.a"
-- (λa. a)

-- >>> parse' "λa.a a"
-- (λa. a a)

-- >>> parse' "λa. a xx ax xx"
-- (λa. a xx ax xx)

-- >>> (groupExpressions . tokenize) "λa. a xx ax xx"
-- >>> interpret it
-- [λ.a,[ a ,[ xx ,[ ax , xx ]]]]
-- (λa. a xx ax xx)



-- leftovers:
-- working simple non-complete examples
{-
-- pass in any variable name, get a correct Var Term expression returned
parse' :: String -> Q TH.Exp
parse' txt = do
  x :: Exp <- TH.litE $ TH.StringL $ removeSpaces txt
  y :: Exp <- [|Var|]
  return $ TH.AppE y x

-- just returns a string literal as haskell expression
parse'' :: String -> Q TH.Exp
parse'' txt = do
  return $ TH.LitE $ TH.StringL txt

removeSpaces :: [Char] -> [Char]
removeSpaces = filter (not . isSpace)
-}
