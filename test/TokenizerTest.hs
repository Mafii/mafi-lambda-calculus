module TokenizerTest where

import Test.Hspec (Spec, describe, it, shouldBe)
import Tokenizer (Token (LambdaWord, VariableUsageOrBinding), tokenize)

spec :: Spec
spec = do
  describe "Tokenizer" $ do
    it "leading lambda is lambda" $
      tokenize "lambdaa" `shouldBe` [LambdaWord, VariableUsageOrBinding "a"]
    it "Lambda can not be a part of a variable name even without space" $
      tokenize "alambda" `shouldBe` [VariableUsageOrBinding "a", LambdaWord]
