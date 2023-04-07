{-# LANGUAGE ScopedTypeVariables #-}

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified LibTest (spec)
import qualified ParserTest (spec)
import qualified ReducerTest (spec)
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hspec spec

spec :: Spec
spec = do
  describe "Parser Tests" ParserTest.spec
  describe "Reducer Tests" ReducerTest.spec
  describe "Lib Tests" LibTest.spec