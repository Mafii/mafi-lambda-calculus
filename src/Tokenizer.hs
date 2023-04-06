module Tokenizer
  ( Token
      ( ClosingParenthesis,
        FunctionAbstractionDot,
        LambdaCharacter,
        LambdaWord,
        BackslashCharacter,
        Newline,
        OpeningParenthesis,
        Space,
        VariableUsageOrBinding
      ),
    tokenize,
  )
where

import Data.Char (isSpace)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

data Token
  = OpeningParenthesis
  | ClosingParenthesis
  | LambdaCharacter
  | LambdaWord
  | BackslashCharacter
  | VariableUsageOrBinding String
  | FunctionAbstractionDot
  | Space
  | Newline
  deriving (Eq)

instance Show Token where
  show OpeningParenthesis = "("
  show ClosingParenthesis = ")"
  show FunctionAbstractionDot = "<dot>"
  show LambdaCharacter = "λ"
  show LambdaWord = "lambda"
  show BackslashCharacter = "\\"
  show Space = "<space>"
  show Newline = "<newline>"
  show (VariableUsageOrBinding id) = "<var " ++ id ++ ">"

tokenize :: String -> [Token]
tokenize text = tokenize' text []

tokenize' :: String -> [Token] -> [Token]
tokenize' "" xs = xs
-- character λ does not work, encoded variant must be used (but it works in (probably unicode that's why) strings)
-- if weird errors happen, add setLocaleEncoding utf8 to your code ahead of running this tokenizer
tokenize' ('\955' : restOfString) tokens = tokenize' restOfString (tokens ++ [LambdaCharacter])
tokenize' ('l' : 'a' : 'm' : 'b' : 'd' : 'a' : restOfString) tokens = tokenize' restOfString (tokens ++ [LambdaWord])
tokenize' ('\\' : restOfString) tokens = tokenize' restOfString (tokens ++ [BackslashCharacter])
tokenize' ('(' : restOfString) tokens = tokenize' restOfString (tokens ++ [OpeningParenthesis])
tokenize' (')' : restOfString) tokens = tokenize' restOfString (tokens ++ [ClosingParenthesis])
tokenize' ('.' : restOfString) tokens = tokenize' restOfString (tokens ++ [FunctionAbstractionDot])
tokenize' (' ' : restOfString) tokens = tokenize' restOfString (tokens ++ [Space])
tokenize' ('\n' : restOfString) tokens = tokenize' restOfString (tokens ++ [Newline])
tokenize' (s : ss) [] = tokenize' ss [VariableUsageOrBinding [s]]
tokenize' (s : ss) tokens =
  tokenize'
    ss
    ( if isVariable (last tokens)
        then init tokens ++ [appendCharToTokenText (last tokens) s]
        else tokens ++ [VariableUsageOrBinding [s]]
    )

isVariable :: Token -> Bool
isVariable (VariableUsageOrBinding _) = True
isVariable _ = False

appendCharToTokenText :: Token -> Char -> Token
appendCharToTokenText (VariableUsageOrBinding t) c = VariableUsageOrBinding (t ++ [c])
appendCharToTokenText _ _ = error "only defined for variables"

-- >>> setLocaleEncoding utf8
-- >>> tokenize "a"
-- >>> tokenize "λa. a b"
-- >>> tokenize "(λb . (λa. a b)) 5"
-- [<var a>]
-- [λ,<var a>,<dot>,<space>,<var a>,<space>,<var b>]
-- [(,λ,<var b>,<space>,<dot>,<space>,(,λ,<var a>,<dot>,<space>,<var a>,<space>,<var b>,),),<space>,<var 5>]