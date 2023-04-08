import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified LibTest (spec)
import qualified ParserTest (spec)
import qualified ReducerTest (spec)
import qualified StepByStepReducerTest (spec)
import Test.Hspec (Spec, describe, hspec)
import qualified TokenizerTest (spec)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hspec spec

spec :: Spec
spec = do
  describe "Tokenizer Tests" TokenizerTest.spec
  describe "Parser Tests" ParserTest.spec
  describe "Reducer Tests" ReducerTest.spec
  describe "Lib Tests" LibTest.spec
  describe "Step by Step Reducer Tests" StepByStepReducerTest.spec
