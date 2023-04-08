{-# LANGUAGE QuasiQuotes #-}

module StepByStepReducerTest where

import LCQQ (λ)
import StepByStepReducer (ReduceInstruction (..), Strategy (..), reduce)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Step by Step reduction" $ do
    it "Reduction with 0 steps is identity operation with LeftmostInnermost" $ do
      reduce [λ| (λx.x) whatever |] (ReduceInstruction LeftmostInnermost 0) `shouldBe` [λ| (λx.x) whatever |]
    it "Reduction with 0 steps is identity operation with LeftmostOutermost" $ do
      reduce [λ| (λx.x) whatever |] (ReduceInstruction LeftmostOutermost 0) `shouldBe` [λ| (λx.x) whatever |]
    it "Identity lambda is reduced to value when value is applied within one step" $ do
      reduce [λ| (λx.x) whatever |] (ReduceInstruction LeftmostOutermost 1) `shouldBe` [λ| whatever |]
