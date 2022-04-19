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

-- complete BNF syntax (Variable, Function Abstraction, Application):
-- <λexp> ::= <var>
--          | λ<var> . <λexp>
--          | ( <λexp> <λexp> )

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

tokenize :: String -> [Token]
tokenize text = tokenize' text []

tokenize' :: String -> [Token] -> [Token]
tokenize' "" xs = xs
-- character λ does not work, encoded variant must be used
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

lambdaCharacter :: Char
lambdaCharacter = head "λ"

testCharacterFunc :: Char -> Bool
testCharacterFunc '\955' = True
testCharacterFunc _ = False

-- >>> setLocaleEncoding utf8 
-- >>> tokenize "a"
-- >>> tokenize "λa. a b"
-- >>> tokenize "(λb . (λa. a b)) 5"
-- [<var a>]
-- [λ,<var a>,<dot>,<space>,<var a>,<space>,<var b>]
-- [(,λ,<var b>,<space>,<dot>,<space>,(,λ,<var a>,<dot>,<space>,<var a>,<space>,<var b>,),),<space>,<var 5>]




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
