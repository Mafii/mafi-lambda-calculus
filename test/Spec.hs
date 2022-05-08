{-# LANGUAGE ScopedTypeVariables #-}

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import LCQQ (lambda, λ)
import Lib (Term (Abs, App, Var))
import qualified LibTest (spec)
import qualified ParserTest (spec)
import qualified ReducerTest (spec)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "(New)Parser Tests" ParserTest.spec
  describe "Reducer Tests" ReducerTest.spec
  describe "Lib Tests" LibTest.spec