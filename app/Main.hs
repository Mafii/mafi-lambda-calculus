module Main where

import Lib
import Parser (parse)
import Tokenizer (tokenize)

main :: IO ()
main = do
  putStrLn "\ESC[92mInsert Term" -- gree color
  term <- getLine
  putStr "\ESC[0mDebug trace:" -- default color
  let tokenized = tokenize term
  print tokenized
  let parsed = parse tokenized
  let test = parsed
  print test -- writes the debug trace + the print into the console
  putStrLn $ "\ESC[33mResult for '" ++ term ++ "':" -- yellow
  print test -- writes the colored result into the console
  main >>= print
  putStrLn "Ciao (you will never see this)"