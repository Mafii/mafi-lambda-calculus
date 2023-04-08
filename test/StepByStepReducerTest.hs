{-# LANGUAGE QuasiQuotes #-}

module StepByStepReducerTest where

import LCQQ (λ)
import Lib (Term (..))
import qualified Reducer (alphaEq)
import StepByStepReducer (MaxSteps, Strategy (..), reduce)
import Test.Hspec (Spec, describe, it, shouldBe)

-- No test should reach that complexity anyways
maxReductionAttempts = 20

spec :: Spec
spec = do
  spec' LeftmostOutermost maxReductionAttempts
  spec' LeftmostInnermost maxReductionAttempts
  describe "Leftmost Outermost Single Step Verification" $ do
    it "Steps are equal to reference example" $ do
      let strategy = LeftmostOutermost
      let initial = [λ| (λa.a) ((λa.a) x)((λb.b) y) |]
      let firstReduction = [λ| ((λa.a) x)((λb.b) y) |]
      reduce initial strategy 1 `shouldBe` firstReduction
      let secondReduction = [λ| (x)((λb.b) y) |]
      reduce firstReduction strategy 1 `shouldBe` secondReduction
      let thirdReduction = [λ| (x)(y) |]
      reduce secondReduction strategy 1 `shouldBe` thirdReduction
  describe "Leftmost Innermost Single Step Verification" $ do
    it "Steps are equal to reference example" $ do
      let strategy = LeftmostInnermost
      let initial = [λ| (λa.a) ((λa.a) x)((λb.b) y) |]
      let firstReduction = [λ| (λa.a) (x)((λb.b) y) |]
      reduce initial strategy 1 `shouldBe` firstReduction
      let secondReduction = [λ| (λa.a) (x)(y) |]
      reduce firstReduction strategy 1 `shouldBe` secondReduction
      let thirdReduction = [λ| (x)(y) |]
      reduce secondReduction strategy 1 `shouldBe` thirdReduction

spec' :: Strategy -> MaxSteps -> Spec
spec' strategy maxSteps = do
  describe ("Step by Step reduction with strategy " ++ show strategy) $ do
    it "Reduction with 0 steps is identity operation" $ do
      reduce
        [λ| (λx.x) whatever |]
        strategy
        0
        `shouldBe` [λ| (λx.x) whatever |]
    it "Identity lambda is reduced to value when value is applied within one step" $ do
      reduce [λ| (λx.x) whatever |] strategy 1 `shouldBe` [λ| whatever |]
    it "Identity lambda reduction still works with higher max steps allowed" $ do
      reduce' [λ| (λx.x) whatever |] `shouldBe` [λ| whatever |]
    it "Endless recursion stops correctly at max steps" $ do
      reduce' [λ| (λx.x x) (λx.x x) |] `shouldBe` [λ| (λx.x x) (λx.x x) |]
    it "Other endless recursion example terminates correctly" $ do
      reduce' [λ| (λx.λy.y) |] `shouldBe` [λ| (λx.λy.y) |]
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
  where
    reduce' lc = reduce lc strategy maxSteps
