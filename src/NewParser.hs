module NewParser (parse, Token (VarUseOrBind, OpenParens, ClosingParens, Dot, Lambda)) where

import Control.Applicative (Alternative ((<|>)), Applicative (liftA2))
import Data.Either (fromLeft)
import qualified Data.Foldable
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Lib (Id, Term (Abs, App, Var))
import qualified Lib
import ParserMonad
  ( Parser,
    Token (..),
    createParser,
    getClosingParenthesis,
    getLambdaVar,
    getOpeningParenthesis,
    getVar,
    isCompleted,
    runParser,
  )
import ResultMonad (R (Error, Ok), fromOk)
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

parseVar :: Parser Term
parseVar = createParser $ \s -> do
  (id, ts) <- runParser getVar s
  Ok (Var id, ts)

parseApp :: Parser Term
parseApp = createParser $ \s -> do
  (lhs, ts) <- runParser getNext s
  (rhs, ts) <- runParser getNext ts
  extend (App lhs rhs) ts
  where
    getNext = parseScope <|> parseAbs <|> parseVar

extend :: Term -> [Token] -> R (Term, [Token])
extend term ts = until isDone runGetNext (Ok (term, ts))
  where
    isDone (Ok (t, s)) = isNextBracket s || null s
    isDone (Error e) = True
    runGetNext original@(Ok (t, s)) = do
      let try =
            ( do
                (el, ts) <- runParser parser s -- might need non-app getnext from parseApp, maybe bug
                Ok (App t el, ts)
            )
      try <|> original
    runGetNext (Error e) = Error e
    isNextBracket s = case runParser getClosingParenthesis s of
      Ok (v, ts) -> True
      Error e -> False

parseAbs :: Parser Term
parseAbs = createParser $ \s -> do
  (id, ts) <- runParser getLambdaVar s -- ensures (lambda id .) as triplet, returns only the id as the rest is useless
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
