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
import qualified Parser (parse)
import ResultMonad (fromOk)
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
parse = toExp . fromOk . Parser.parse

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

-- >>> toExp $ fromOk $ (NewParser.parse "lambda a. a b")
-- AppE (AppE (ConE Lib.Abs) (ListE [LitE (CharL 'a')])) (AppE (AppE (ConE Lib.App) (AppE (ConE Lib.Var) (ListE [LitE (CharL 'a')]))) (AppE (ConE Lib.Var) (ListE [LitE (CharL 'b')])))
