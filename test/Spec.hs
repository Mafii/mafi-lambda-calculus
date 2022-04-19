{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

import LCQQ (lambda, λ)
import Lib (Term (Abs, Var))
import Test.Hspec ( hspec, describe, it, shouldBe, Spec )
import GHC.IO.Encoding (setLocaleEncoding, utf8)

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

-- do
--describe "QuasiQuotes for LC" $ do
-- it "QQ with unicode lambda is equal to lambda" $
--   [λ| asdf |] == [lambda| asdf |]
-- it "QQ returns LC Term" $
--   (Var "abcd") == [lambda| abcd |]
--return
--  ()