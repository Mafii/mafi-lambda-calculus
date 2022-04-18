{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

import LCQQ (lambda, λ)
import Lib (Term (Var))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "QuasiQuotes for LC" $ do
    it "Creates Var correctly" $
      ([λ| x |] :: Term) `shouldBe` (Var "x")
    it "lambda and unicode lambda behave the same" $
      ([λ| x |] :: Term) `shouldBe` ([lambda| x |] :: Term)

-- do
--describe "QuasiQuotes for LC" $ do
-- it "QQ with unicode lambda is equal to lambda" $
--   [λ| asdf |] == [lambda| asdf |]
-- it "QQ returns LC Term" $
--   (Var "abcd") == [lambda| abcd |]
--return
--  ()