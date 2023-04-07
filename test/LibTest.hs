{-# LANGUAGE QuasiQuotes #-}

module LibTest where

import LCQQ (λ)
import Lib (showPretty)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Pretty-Print" $ do
    it "Pretty prints lambda in application with bracket" $ do
      showPretty [λ| (λx.x y z a b) c |] `shouldBe` "(λx.x y z a b) c"
    it "Brackets relevant for precedence are not lost" $ do
      showPretty [λ| a (b c) d |] `shouldBe` "a (b c) d"
    it "Brackets are omitted when irrelevant" $ do
      showPretty [λ| (b c) d |] `shouldBe` "b c d"
    it "Does not bracket inner lambda expressions" $ do
      showPretty [λ| λx.λy.x |] `shouldBe` "λx.λy.x"