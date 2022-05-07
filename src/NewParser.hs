module NewParser (parse, TermOrScope (T, S), Token (VarUseOrBind, OpenParens, ClosingParens, Dot, Lambda)) where

import Control.Applicative (Alternative ((<|>)))
import Data.Either (fromLeft)
import qualified Data.Foldable
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Lib (Id, Term (Abs, App, Var))
import qualified Lib
import ParserMonad (Parser, Token (..), getOpeningParenthesis, runParser)
import ResultMonad (R (Error, Ok), fromOk)
import Tokenizer (tokenize)
import qualified Tokenizer (Token (..), tokenize)

infixl 0 |> -- ($) has infixr 0

(|>) :: b -> (b -> c) -> c
(|>) = flip ($)

parse :: String -> R Term
parse text = text |> tokenize |> flatMapTokens |> runParser parser |> completedOrError |> (<&> toTerm)

-- basically a sanity check
completedOrError :: (Show a) => R (a, [Token]) -> R a
completedOrError (Ok (a, [])) = Ok a
completedOrError (Ok (a, ts)) = Error $ "Partial result: " ++ show a ++ ", but unhandled tokens: " ++ show ts
completedOrError (Error a) = Error a

parser :: Parser TermOrScope
parser = parseScope <|> parseApp <|> parseAbs <|> parseVar

parseScope :: Parser TermOrScope
parseScope = do
  --(_, ts) <- getOpeningParenthesis
  undefined

parseVar :: Parser TermOrScope
parseVar = undefined

parseApp :: Parser TermOrScope
parseApp = undefined

parseAbs :: Parser TermOrScope
parseAbs = undefined

-- infrastructure

type Depth = Int

data TermOrScope -- can be converted to term
  = T Term -- Can be extended
  | S TermOrScope Depth -- Can not be extended
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