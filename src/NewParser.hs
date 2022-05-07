module NewParser (parse, TermOrScope (T, S), Token (VarUseOrBind, OpenParens, ClosingParens, Dot, Lambda)) where

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

-- >>> parse "lambda a . a"

parse :: String -> R Term
parse text = text |> tokenize |> flatMapTokens |> runParser (parser 0) |> completedOrError |> (<&> toTerm)

-- basically a sanity check
completedOrError :: (Show a) => R (a, [Token]) -> R a
completedOrError (Ok (a, [])) = Ok a
completedOrError (Ok (a, ts)) = Error $ "Partial result: " ++ show a ++ ", but unhandled tokens: " ++ show ts
completedOrError (Error a) = Error a

parser :: Depth -> Parser TermOrScope
parser = parseScope <||> parseApp <||> parseAbs <||> parseVar

-- so we can avoid passing depth to every call in the parser function
(<||>) :: (Alternative f1, Applicative f2) => f2 (f1 a) -> f2 (f1 a) -> f2 (f1 a)
(<||>) = liftA2 (<|>)

parseScope :: Depth -> Parser TermOrScope
parseScope depth = createParser $ \s -> do
  ((), ts) <- runParser getOpeningParenthesis s
  (scopeContent, ts') <- runParser (parser (depth + 1)) ts
  Ok (S scopeContent, ts')

parseVar :: Depth -> Parser TermOrScope
parseVar depth = createParser $ \s -> do
  (id, ts) <- runParser getVar s
  Ok (T $ Var id, ts)

parseApp :: Depth -> Parser TermOrScope
parseApp depth = createParser $ \s -> do
  (lhs, ts) <- runParser (getNext depth) s
  (rhs, ts) <- runParser (getNext depth) ts
  Ok (T $ App (toTerm lhs) (toTerm rhs), ts) -- non-finished try
  -- until isDone runGetNext (Ok (T $ App (toTerm lhs) (toTerm rhs), ts))
  where
    isDone (Ok (t, s)) = isNextBracket s || null s
    isDone (Error e) = True
    getNext = parseScope <||> parseAbs <||> parseVar
    runGetNext original@(Ok (t, s)) = do
      let try =
            ( do
                (el, ts) <- runParser (getNext depth) s
                Ok (T $ App (toTerm t) (toTerm el), ts)
            )
      try <|> original
    runGetNext (Error e) = Error e
    isNextBracket s = case runParser getClosingParenthesis s of
      Ok (v, ts) -> True
      Error e -> False

parseAbs :: Depth -> Parser TermOrScope
parseAbs depth = createParser $ \s -> do
  (id, ts) <- runParser getLambdaVar s -- ensures (lambda id .) as triplet, returns only the id as the rest is useless
  (body, ts') <- runParser (parser depth) ts
  Ok (T (Abs id (toTerm body)), ts')

-- infrastructure

type Depth = Int

data TermOrScope -- can be converted to term
  = T Term -- Can be extended
  | S TermOrScope -- Can not be extended
  deriving (Show)

toTerm :: TermOrScope -> Term
toTerm t = error $ "unimplemented, should convert TermOrScope to Term: " ++ show t

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