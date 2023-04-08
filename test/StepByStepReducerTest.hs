{-# LANGUAGE QuasiQuotes #-}

module StepByStepReducerTest where

import LCQQ (λ)
import Lib (Term (..))
import qualified Reducer (alphaEq)
import StepByStepReducer (Strategy (..), reduce)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Step by Step reduction" $ do
    it "Reduction with 0 steps is identity operation with LeftmostInnermost" $ do
      reduce [λ| (λx.x) whatever |] LeftmostInnermost 0 `shouldBe` [λ| (λx.x) whatever |]
    it "Reduction with 0 steps is identity operation with LeftmostOutermost" $ do
      reduce [λ| (λx.x) whatever |] LeftmostOutermost 0 `shouldBe` [λ| (λx.x) whatever |]
    it "Identity lambda is reduced to value when value is applied within one step" $ do
      reduce [λ| (λx.x) whatever |] LeftmostOutermost 1 `shouldBe` [λ| whatever |]
    it "Endless recursion stops correctly at max steps" $ do
      reduce [λ| (λx.x x) (λx.x x) |] LeftmostOutermost 50 `shouldBe` [λ| (λx.x x) (λx.x x) |]
  describe "General reduction tests" $ do
    it "Identity lambda is reduced to value when value is applied" $ do
      reduce' [λ| (λx.x) whatever |] `shouldBe` [λ| whatever |]
    it "`(λx.x x) (λx.x)` reduces to `λx.x`" $ do
      reduce' [λ| (λx.x x) (λx.x) |] `shouldBe` [λ| λx.x |]
    it "sanity check with manual ast" $ do
      reduce' (App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (Var "x"))) == Abs "x" (Var "x")
    it "`(λx.λy.x y) y` reduces to `λy1.y y1`, and not `λy.y y` (hint: may be brittle, equivalent to next test" $ do
      reduce' [λ| (λx.λy.x y) y |] `shouldBe` [λ| λy1.y y1 |]
    it "`(λx.λy.x y) y` reduces to a term that is alpha equivalent to `λz.y z`" $ do
      Reducer.alphaEq (reduce' (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))) (Abs "z" (App (Var "y") (Var "z")))

-- No test should reach that complexity anyways
maxReductionAttempts = 20

-- makes general tests easier for now - once leftmost innermost is implemented, this has to be changed
reduce' :: Term -> Term
reduce' term = reduce term LeftmostOutermost maxReductionAttempts