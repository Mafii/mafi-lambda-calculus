{-# LANGUAGE TemplateHaskellQuotes #-}

-- Inspired by https://github.com/tweag/HaskellR/blob/ace283d47a89d680d03182461f4dba98da2ee042/inline-r/src/Language/R/QQ.hs
module LCQQ (λ, lambda) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp, Q)
import qualified Language.Haskell.TH.Syntax as TH
import Lib (Term (Abs, App, Var))
import qualified Parser (parse)
import ResultMonad (fromOk)

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
parse = toExp . fromOk . Parser.parse

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
