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
    getNextToken,
    getOpeningParenthesis,
    getVar,
    isCompleted,
    runParser,
  )
import ResultMonad (R (Error, Ok), fromOk, orElse)
import Tokenizer (tokenize)
import qualified Tokenizer (Token (..), tokenize)

parse :: String -> R Term
parse = completedOrError . runParser parser . flatMapTokens . tokenize

-- >>> parse "a b c (d e) f"
-- >>> parse "((((a))b))c"
-- >>> parse "(lambda a . (lambda b . b 7 8) 5) 99"
-- ((a b) ((c (d e)) f))
-- ((a b) c)
-- ((λa. ((λb. ((b 7) 8)) 5)) 99)

parser :: Parser Term
parser = parseApp <|> parseScope <|> parseAbs <|> parseVar

parseScope :: Parser Term
parseScope = getOpeningParenthesis *> parser <* getClosingParenthesis

parseVar :: Parser Term
parseVar = Var <$> getVar

appL :: (Applicative f) => f Term -> f Term -> f Term
appL = liftA2 App

parseApp :: Parser Term
parseApp = createParser $ \ts -> do
  (lhs, ts) <- runParser getNext ts
  (rhs, ts) <- runParser getNext ts
  extend (App lhs rhs) ts
  where
    getNext = parseScope <|> parseAbs <|> parseVar
    extend term ts = until isDone extend' (Ok (term, ts))
    extend' :: R (Term, [Token]) -> R (Term, [Token])
    extend' state = state >>= \(lhs, ts) -> runParser parser ts >>= \(rhs, ts) -> Ok (App lhs rhs, ts)
    isDone :: R (Term, [Token]) -> Bool
    isDone s =
      let tokens = state s
          isNextBracketOrNull = fmap $ liftA2 (||) isNextBracket null
       in isNextBracketOrNull tokens `orElse` False
    isNextBracket :: [Token] -> Bool
    isNextBracket s = runParser getClosingParenthesis s $> True `orElse` False
    state :: R (Term, [Token]) -> R [Token]
    state (Ok (t, ts)) = Ok ts
    state (Error e) = Error e

g

absL :: (Applicative f) => f Id -> f Term -> f Term
absL = liftA2 Abs

parseAbs :: Parser Term
parseAbs = absL getLambdaVar parser

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
