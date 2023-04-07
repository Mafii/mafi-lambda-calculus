{-# LANGUAGE InstanceSigs #-}

module ParserMonad
  ( Parser (..),
    Token (..),
    getNextToken,
    getLambdaVar,
    getVar,
    getOpeningParenthesis,
    getClosingParenthesis,
    isCompleted,
    runParser,
    createParser,
  )
where

import Control.Applicative (Alternative)
import Control.Monad.Trans.State.Strict (StateT (StateT, runStateT))
import Data.Functor ((<&>))
import GHC.Base (Alternative (empty, (<|>)))
import Lib (Id, Term (..))
import ResultMonad (R (..))

data Token
  = OpenParens
  | Dot
  | Lambda
  | ClosingParens
  | VarUseOrBind String
  deriving (Show)

newtype Parser a = Parser {unparser :: StateT [Token] R a}

createParser :: ([Token] -> R (a, [Token])) -> Parser a
createParser f = Parser $ StateT f

getNextToken :: Parser Token
getNextToken = createParser $ \s -> do
  case s of
    [] -> Error "No tokens left but getNextToken called"
    (t : ts) -> Ok (t, ts)

getLambdaVar :: Parser Id
getLambdaVar = createParser $ \s -> do
  case s of
    (Lambda : VarUseOrBind id : Dot : tokens) -> Ok (id, tokens)
    ts -> Error $ "Expected abstraction start 'λa.' but got: " ++ show ts

getVar :: Parser Id
getVar = createParser $ \s -> do
  (t, ts) <- runParser getNextToken s
  case t of
    VarUseOrBind id -> Ok (id, ts)
    t' -> Error $ "Expected variable but got: " ++ show t'

getOpeningParenthesis :: Parser ()
getOpeningParenthesis = createParser $ \s -> do
  (t, ts) <- runParser getNextToken s
  case t of
    OpenParens -> Ok ((), ts)
    t' -> Error $ "Expected opening parenthesis but got: " ++ show t'

getClosingParenthesis :: Parser ()
getClosingParenthesis = createParser $ \s -> do
  (t, ts) <- runParser getNextToken s
  case t of
    ClosingParens -> Ok ((), ts)
    t' -> Error $ "Expected closing parenthesis but got: " ++ show t'

isCompleted :: Parser Bool
isCompleted = createParser $ \s -> do
  case s of
    [] -> Ok (True, [])
    ts -> Ok (False, ts)

instance Functor Parser where
  fmap f p = Parser $ f <$> unparser p

instance Applicative Parser where
  pure a = Parser $ pure a
  f <*> a = Parser $ unparser f <*> unparser a

instance Alternative Parser where
  empty = empty -- only works because of monad plus on R. Otherwise: Parser $ StateT $ const empty
  a <|> b = Parser $ unparser a <|> unparser b

runParser :: Parser a -> [Token] -> R (a, [Token])
runParser = runStateT . unparser

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  a >>= fa = Parser $
    StateT $ \s -> do
      (a', s') <- runParser a s
      runParser (fa a') s'
