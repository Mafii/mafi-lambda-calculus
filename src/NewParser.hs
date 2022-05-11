{-# LANGUAGE InstanceSigs #-}

module NewParser (parse, Token (VarUseOrBind, OpenParens, ClosingParens, Dot, Lambda)) where

import Control.Applicative (Alternative ((<|>)), Applicative (liftA2), liftA, liftA3)
import Data.Either (fromLeft)
import qualified Data.Foldable
import Data.Functor (($>), (<&>))
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Lib (Id, Term (Abs, App, Var))
import qualified Lib
import ParserMonad
  ( Parser (unparser),
    Token (..),
    createParser,
    getClosingParenthesis,
    getLambdaVar,
    getOpeningParenthesis,
    getVar,
    isCompleted,
    runParser,
  )
import ResultMonad (R (Error, Ok), fromOk, orElse)
import Tokenizer (tokenize)
import qualified Tokenizer (Token (..), tokenize)

infixl 0 |> -- ($) has infixr 0

-- I personally think the pipe forward operator ((|>), also found in F# iirc) is superior over function composition (.)
-- and the $ operator as (at least in europe) humans read from left to right.
(|>) :: b -> (b -> c) -> c
(|>) = flip ($)

parse :: String -> R Term
parse text = text |> tokenize |> flatMapTokens |> runParser parser |> completedOrError

-- >>> parse "a b c (d e) f"
-- >>> parse "((((a))b))c"
-- >>> parse "(lambda a . (lambda b . b 7 8) 5) 99"
-- ((a b) ((c (d e)) f))
-- ((a b) c)
-- ((λa. ((λb. ((b 7) 8)) 5)) 99)

parser :: Parser Term
parser = parseApp <|> parseScope <|> parseAbs <|> parseVar

parseScope :: Parser Term
parseScope = createParser $ \ts -> do
  ((), ts) <- runParser getOpeningParenthesis ts
  (scopeContent, ts) <- runParser parser ts
  ((), ts) <- runParser getClosingParenthesis ts
  Ok (scopeContent, ts)

pS :: Parser Term
pS = getOpeningParenthesis *>> parser <<* getClosingParenthesis

g

-- pS = getOpeningParenthesis >>> parser <<* undefined

-- idea: Behaves similar to (<*): gets the result of the left side, calculates the right side and
-- returns the left side with the state left after calculating the right side.
(<<*) :: Parser a -> Parser b -> Parser a
(<<*) p1 p2 = createParser $ \ts -> do
  (result, ts) <- runParser p1 ts
  (_, ts) <- runParser p2 ts
  Ok (result, ts)

-- idea: Behaves similar to (*>): gets the result of the left side and discards it,
-- then calculates the result of the right side based on the state
(*>>) :: Parser a -> Parser b -> Parser b
(*>>) p1 p2 = createParser $ \ts -> do
  (_, ts) <- runParser p1 ts
  (result, ts) <- runParser p2 ts
  Ok (result, ts)

parseVar :: Parser Term
parseVar = Var <$> getVar

parseApp :: Parser Term
parseApp = appL getNext getNext |> extend
  where
    getNext = parseScope <|> parseAbs <|> parseVar

extend :: Parser Term -> Parser Term
extend p = createParser $ \s -> do
  (lhs, s) <- runParser p s
  let done = null s || isNextBracket s
  if done
    then Ok (lhs, s)
    else do
      (rhs, s) <- runParser parser s
      Ok (App lhs rhs, s)
  where
    isNextBracket :: [Token] -> Bool
    isNextBracket s = runParser getClosingParenthesis s $> True `orElse` False

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = liftA2 (||)

state :: R (Term, [Token]) -> R [Token]
state (Ok (t, ts)) = Ok ts
state (Error e) = Error e

-- can be read as "create parser and put its result into" - this has been a repeating pattern

-- (*->) :: Parser a -> (a -> b) -> Parser b
-- (*->) p f = createParser $ \s -> do
--   (r, ts) <- runParser p s
--   Ok (f r, ts)

-- when generalizing the signature away from "Term" I realized this is almost fmap, <$> or <*>
-- fmap :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b
-- (from hoogle: (<*>) = liftA2 id)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b

-- this "proofs" that (*->) could be flip (<$>):
-- >>> runParser (getVar *-> Var) [VarUseOrBind "a"]
-- >>> runParser (Var <$> getVar) [VarUseOrBind "a"]
-- (a,[])
-- (a,[])

-- as (<$>) is function application ($) lifted over a functor,
-- and (|>) is flip ($) in my module, maybe (<|>>) would be  a good idea (but (<|>) is already defined in Alternative)
-- so maybe (|>>) ?

(|>>) :: (Functor f) => f a -> (a -> b) -> f b
-- (|>>) v f = fmap f v -- would also work as <$> is the infix operator of fmap
(|>>) = flip (<$>)

appL :: (Functor f, Applicative f) => f Term -> f Term -> f Term
appL = liftA2 App

-- >>> let parser = getVar |>> Var |> appL (Var <$> getVar)
-- >>> let parser2 = appL (Var <$> getVar) (Var <$> getVar)
-- >>> runParser parser [VarUseOrBind "a", VarUseOrBind "b"]
-- >>> runParser parser2 [VarUseOrBind "a", VarUseOrBind "b"]
-- ((a b),[])
-- ((a b),[])

(>>>) :: Parser a -> (a -> b) -> [Token] -> R (b, [Token])
(>>>) p f tokens = runParser p tokens >>= \(result, ts) -> Ok (f result, ts)

-- evalAndDiscard :: Parser a -> (a -> b) -> [Token] -> R (a, [Token])
-- evalAndDiscard p f tokens = runParser p tokens >>= \(result, ts) -> Ok (result, ts)

-- (&>>) :: Parser Term -> (Term -> Term -> Term) -> R (Term, [Token]) -> R (Term, [Token])
-- (&>>) p f state = state >>= \(lhs, ts) -> runParser p ts >>= \(rhs, ts) -> Ok (f lhs rhs, ts)

-- >>> Error "hi" *> pure True
-- >>> Ok ((), []) *> pure True
-- >>> Error "asdf" *> pure True <|> pure False
-- >>> Error "abc" $> True `orElse` False
-- >>> Ok "abc" $> True `orElse` False
-- hi
-- True
-- False
-- False
-- True

parseAbs :: Parser Term
parseAbs = createParser $ \s -> do
  (id, ts) <- runParser getLambdaVar s
  (body, ts') <- runParser parser ts
  Ok (Abs id body, ts')

-- infrastructure

flatMapTokens :: [Tokenizer.Token] -> [Token]
flatMapTokens = flatMap mapToken
  where
    flatMap f = concatMap (Data.Foldable.toList . f)

mapToken :: Tokenizer.Token -> Maybe Token
mapToken Tokenizer.OpeningParenthesis = Just OpenParens
mapToken Tokenizer.ClosingParenthesis = Just ClosingParens
mapToken Tokenizer.FunctionAbstractionDot = Just Dot
mapToken Tokenizer.LambdaCharacter = Just Lambda
mapToken Tokenizer.LambdaWord = Just Lambda
mapToken Tokenizer.Newline = Nothing
mapToken Tokenizer.Space = Nothing
mapToken (Tokenizer.VariableUsageOrBinding id) = Just $ VarUseOrBind id

-- basically a sanity check
completedOrError :: (Show a) => R (a, [Token]) -> R a
completedOrError (Ok (a, [])) = Ok a
completedOrError (Ok (a, ts)) = Error $ "Partial result: " ++ show a ++ ", but unhandled tokens: " ++ show ts
completedOrError (Error a) = Error a
