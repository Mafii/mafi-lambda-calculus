module Main where

import Lib
import Parser (parse)
import Tokenizer (tokenize)

main :: IO ()
main = do
  putStrLn "Insert Term"
  term <- getLine
  let tokenized = tokenize term
  putStrLn $ show tokenized
  let parsed = parse tokenized
  putStrLn $ show parsed
  next <- main
  putStrLn "Ciao"