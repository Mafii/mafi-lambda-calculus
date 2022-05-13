module Main where

import Lib
import Parser (parse)
import Tokenizer (tokenize)

main :: IO ()
main = do
  putStrLn "\ESC[92mInsert Term" -- gree color
  term <- getLine
  putStrLn $ "\ESC[33m" ++ show (parse term)
  main >>= print
  putStrLn "Ciao (you will never see this)"