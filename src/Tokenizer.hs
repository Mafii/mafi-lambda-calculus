module Tokenizer (tokenize, Token (..)) where

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
  | Comment String
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
  show (Comment s) = "#" ++ s

tokenize :: String -> [Token]
tokenize "" = []
tokenize ('#' : rest) = Comment commentText : tokenize rest'
  where
    (commentText, rest') = break (== '\n') rest
tokenize ('\955' : rest) = LambdaCharacter : tokenize rest
tokenize ('l' : 'a' : 'm' : 'b' : 'd' : 'a' : rest) = LambdaWord : tokenize rest
tokenize ('\\' : rest) = BackslashCharacter : tokenize rest
tokenize ('(' : rest) = OpeningParenthesis : tokenize rest
tokenize (')' : rest) = ClosingParenthesis : tokenize rest
tokenize ('.' : rest) = FunctionAbstractionDot : tokenize rest
tokenize (' ' : rest) = Space : tokenize rest
tokenize ('\n' : rest) = Newline : tokenize rest
tokenize (c : rest) = case tokenize rest of
  ((VariableUsageOrBinding id) : ts) -> VariableUsageOrBinding (c : id) : ts
  ts -> VariableUsageOrBinding [c] : ts
