{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser (parse, Ast) where

import qualified Data.Foldable (toList)
import qualified Tokenizer
  ( Token
      ( ClosingParenthesis,
        FunctionAbstractionDot,
        LambdaCharacter,
        LambdaWord,
        Newline,
        OpeningParenthesis,
        Space,
        VariableUsageOrBinding
      ),
    tokenize,
  )

data Token
  = OpenParens
  | Dot
  | Lambda
  | ClosingParens
  | VarUseOrBind String
  deriving (Show)

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

data Ast
  = Var String
  | Abs String Ast
  | App Ast Ast
  | Empty
  deriving (Show)

data AbortReason
  = NoTokens
  | Parenthesis
  deriving (Show)

data PartialResult a = PartialResult
  { result :: a,
    unhandledTokens :: [Token]
  }
  deriving (Show)

data Abort = Abort
  { reason :: AbortReason,
    leftoverTokens :: [Token]
  }
  deriving (Show)

type Until = AbortReason

class ParseResult result ast where
  nextToken :: result -> Either Abort Token
  tokens :: result -> [Token]
  getAst :: result -> ast

class (ParseResult result ast) => Extendable result ast where
  extend :: PartialResult ast -> Until -> result

instance (Show ast) => ParseResult (PartialResult ast) ast where
  nextToken (PartialResult ast []) = Left (Abort NoTokens [])
  nextToken (PartialResult ast (x : xs)) = Right x
  tokens = unhandledTokens
  getAst (PartialResult ast []) = ast
  getAst (PartialResult _ ts) = error "unhandled tokens"

instance (Show ast) => Extendable (PartialResult ast) ast where
  extend partialResult until = undefined

instance ParseResult Abort ast where
  nextToken (Abort r []) = Left (Abort NoTokens [])
  nextToken (Abort r (x : xs)) = Right x
  tokens = leftoverTokens
  getAst = error "undhandled abort"

-- basically something like a result or error monad
data Result a = Ok a | Error AbortReason
  deriving (Show)

instance Monad Result where
  return = Ok
  (>>=) (Ok a) f = f a
  (>>=) (Error r) f = Error r

instance Functor Result where
  fmap f ra = ra >>= return . f

instance Applicative Result where
  pure = return
  (<*>) mapr ra = do
    f <- mapr
    a <- ra
    pure $ f a

fromOk :: Result a -> a
fromOk (Ok a) = a
fromOk (Error r) = error $ show r

parse :: [Tokenizer.Token] -> Ast
parse ts = getAst $ fromOk $ parse' $ return $ PartialResult Empty (flatMapTokens ts)

parse' :: Result (PartialResult Ast) -> Result (PartialResult Ast)
parse' r = r >>= parse''

parse'' :: PartialResult Ast -> Result (PartialResult Ast)
parse'' = undefined

-- manual tests to learn how to think about monads

testExample :: a -> [Token] -> Result (PartialResult a)
testExample a b = Ok $ PartialResult a b

resultValue :: Result (PartialResult Ast)
resultValue = testExample (Var "a") [Lambda]

getAstFromResult :: PartialResult Ast -> Ast
getAstFromResult (PartialResult ast tkns) = ast

-- >>> :t (resultValue >>=)
-- (resultValue >>=) :: (PartialResult Ast -> Result b) -> Result b

-- >>> resultValue >>= return . getAstFromResult
-- Ok (Var "a")

-- >>> Error NoTokens >>= return . getAstFromResult
-- Error NoTokens

-- >>> :t (Error NoTokens >>= return . getAstFromResult >>=)
-- (Error NoTokens >>= return . getAstFromResult >>=) :: (Ast -> Result b) -> Result b

-- >>> Error NoTokens >>= return . getAstFromResult >>= return . Done
-- Error NoTokens
