module Parser (parse, Token (..)) where

import qualified Data.Foldable
import Control.Applicative (some, liftA2, (<|>))
import Lib (Term (Abs, App, Var))
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
import ResultMonad (R (Error, Ok))
import Tokenizer (tokenize)
import qualified Tokenizer (Token (..))

parse :: String -> R Term
parse = ensureComplete . runParser parser . flatMapTokens . tokenize

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
mapToken Tokenizer.BackslashCharacter = Just Lambda
mapToken Tokenizer.Newline = Nothing
mapToken Tokenizer.Space = Nothing
mapToken (Tokenizer.VariableUsageOrBinding id) = Just $ VarUseOrBind id

ensureComplete :: (Show a) => R (a, [Token]) -> R a
ensureComplete (Ok (a, [])) = Ok a
ensureComplete (Ok (a, ts)) = Error $ "Partial result: " ++ show a ++ ", but unhandled tokens: " ++ show ts
ensureComplete (Error a) = Error a
