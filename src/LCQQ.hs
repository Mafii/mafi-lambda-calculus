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

λ :: QuasiQuoter
λ =
  QuasiQuoter
    { quoteExp = parse',
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

parseVariable :: String -> Q TH.Exp
parseVariable str = undefined

-- leftovers:
-- working simple non-complete examples

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
