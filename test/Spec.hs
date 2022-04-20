{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import LCQQ (lambda, λ)
import Lib (Term (Abs, App, Var))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let _ = setLocaleEncoding utf8
  describe "QuasiQuotes for LC" $ do
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

-- do
--describe "QuasiQuotes for LC" $ do
-- it "QQ with unicode lambda is equal to lambda" $
--   [λ| asdf |] == [lambda| asdf |]
-- it "QQ returns LC Term" $
--   (Var "abcd") == [lambda| abcd |]
--return
--  ()