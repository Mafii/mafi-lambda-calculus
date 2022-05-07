{-# LANGUAGE InstanceSigs #-}

module ParserMonad
  ( Parser,
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

-- I found this article when I struggled with implementing Applicative and Monad for my type
-- The rest of the code is inspired by the paper referenced in the article
-- https://vaibhavsagar.com/blog/2018/02/04/revisiting-monadic-parsing-haskell/
-- I wanted to use R instead of maybe for error messages.
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

-- is this actually obeying the laws?
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

-- manual tests

test :: Parser Term
test = do
  var <- getVar <&> Var
  (getVar <&> App var . Var) <|> pure var

test2 :: Parser Term
test2 = do
  var <- getVar <&> Var
  (getVar <&> Var <&> App var) <|> pure var -- alternative way to write the same thing

-- >>> runParser test [VarUseOrBind "a", VarUseOrBind "a", VarUseOrBind "a"]
-- >>> runParser test [VarUseOrBind "a"]
-- >>> runParser test [OpenParens]
-- ((a a),[VarUseOrBind "a"])
-- (a,[])
-- Expected variable but got: OpenParens
