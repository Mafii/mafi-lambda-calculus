{-# LANGUAGE QuasiQuotes #-}

module ReducerTest where

import LCQQ (λ)
import Lib (Term (Abs, App, Var))
import Reducer (alphaEq, reduce)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Basic requirements for testat" $ do
    it "Identity lambda is reduced to value when value is applied" $ do
      reduce [λ| (λx.x) whatever |] `shouldBe` [λ| whatever |]
    it "`(λx.x x) (λx.x)` reduces to `λx.x`" $ do
      reduce [λ| (λx.x x) (λx.x) |] `shouldBe` [λ| λx.x |]
    it "sanity check with manual ast" $ do
      reduce (App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (Var "x"))) == Abs "x" (Var "x")
    it "`(λx.λy.x y) y` reduces to `λy1.y y1`, and not `λy.y y` (hint: may be brittle, equivalent to next test" $ do
      reduce [λ| (λx.λy.x y) y |] `shouldBe` [λ| λy1.y y1 |]
    it "`(λx.λy.x y) y` reduces to a term that is alpha equivalent to `λz.y z`" $ do
      alphaEq (reduce (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))) (Abs "z" (App (Var "y") (Var "z")))
