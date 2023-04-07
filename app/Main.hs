module Main where

import Lib
import Parser (parse)
import Tokenizer (tokenize)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do
  setLocaleEncoding utf8
  putStrLn "\ESC[92mInsert Term"
  term <- getLine
  putStrLn $ "\ESC[33m" ++ show (parse term)
  main >>= print
  putStrLn "Ciao (you will never see this)"
