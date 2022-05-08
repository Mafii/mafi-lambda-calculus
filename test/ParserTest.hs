{-# LANGUAGE QuasiQuotes #-}

module ParserTest where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import LCQQ (lambda, λ)
import Lib (Term (Abs, App, Var))
import Test.Hspec (Spec, describe, it, shouldBe)

-- uses NewParser over Parser now.
spec :: Spec
spec = do
  let _ = setLocaleEncoding utf8
  describe "QuasiQuotes for LC" $ do
    it "unicode lambda is equal to lambda" $
      [λ| asdf |] == [lambda| asdf |]
    it "Creates Var correctly" $
      ([λ| x |] :: Term) `shouldBe` Var "x"
    it "lambda and unicode lambda behave the same" $
      ([λ| x |] :: Term) `shouldBe` ([lambda| x |] :: Term)
    it "Creates lc abstraction correctly" $
      ([λ| λx. x |] :: Term) `shouldBe` Abs "x" (Var "x")
    it "Allows chained lambdas" $
      ([λ| λx. x λy. x y |] :: Term) `shouldBe` Abs "x" (App (Var "x") (Abs "y" (App (Var "x") (Var "y"))))
    it "Creates nested lc term correctly" $
      ([λ| (λx. x λy. x y) 5 |] :: Term) `shouldBe` App (Abs "x" (App (Var "x") (Abs "y" (App (Var "x") (Var "y"))))) (Var "5")
    it "Nested Parenthesis are handled correctly" $
      ([λ| (a (b) c) |] :: Term) `shouldBe` App (App (Var "a") (Var "b")) (Var "c")
    it "Can start with scope and then have more terms" $
      [lambda| ((((a))b))c |] `shouldBe` App (App (Var "a") (Var "b")) (Var "c")
    it "(lambda a . (lambda b . b 7 8) 5) 99 is handled correctly" $
      [lambda| (lambda a . (lambda b . b 7 8) 5) 99 |]
        `shouldBe` App
          (Abs "a" (App (Abs "b" (App (App (Var "b") (Var "7")) (Var "8"))) (Var "5")))
          (Var "99")