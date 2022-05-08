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

(|>) :: b -> (b -> c) -> c
(|>) = flip ($)

parse :: String -> R Term
parse text = text |> tokenize |> flatMapTokens |> runParser parser |> completedOrError

-- >>> parse "a b c (d e) f"
-- >>> parse "((((a))b))c"
-- >>> parse "lambda a . (lambda b . b 7 8) 5"
-- ((((a b) c) (d e)) f)
-- Expected closing parenthesis but got: VarUseOrBind "b"
-- ((λa. (λb. ((b 7) 8))) 5)

parser :: Parser Term
parser = parseScope <|> parseApp <|> parseAbs <|> parseVar

-- so we can avoid passing depth to every call in the parser function
(<||>) :: (Alternative f1, Applicative f2) => f2 (f1 a) -> f2 (f1 a) -> f2 (f1 a)
(<||>) = liftA2 (<|>)

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
  until isDone runGetNext (Ok (App lhs rhs, ts))
  where
    isDone (Ok (t, s)) = isNextBracket s || null s
    isDone (Error e) = True
    getNext = parseScope <|> parseAbs <|> parseVar
    runGetNext original@(Ok (t, s)) = do
      let try =
            ( do
                (el, ts) <- runParser getNext s
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
