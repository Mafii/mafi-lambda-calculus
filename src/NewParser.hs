module NewParser (parse, Token (VarUseOrBind, OpenParens, ClosingParens, Dot, Lambda)) where

import Control.Applicative (Alternative (many, some, (<|>)), Applicative (liftA2), liftA, liftA3)
import Data.Either (fromLeft)
import qualified Data.Foldable
import Data.Functor (($>), (<&>))
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Lib (Id, Term (Abs, App, Var))
import qualified Lib
import ParserMonad
  ( Parser (Parser, unparser),
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

parser :: Parser Term
parser = foldt <$> some (parseScope <|> parseAbs <|> parseVar)
  where
    foldt :: [Term] -> Term
    foldt [] = error "unreachable, should only be called in combination with some"
    foldt (first : terms) = foldl App first terms

parseScope :: Parser Term
parseScope = getOpeningParenthesis *> parser <* getClosingParenthesis

parseVar :: Parser Term
parseVar = Var <$> getVar

parseAbs :: Parser Term
parseAbs = liftA2 Abs getLambdaVar parser

-- external to internal token handling

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
