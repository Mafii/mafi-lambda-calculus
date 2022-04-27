{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use <&>" #-}

module Parser where

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

data Done a = Done
  { done :: a
  }
  deriving (Show)

type Until = AbortReason

class ParseResult result where
  nextToken :: result -> Either Abort Token
  tokens :: result -> [Token]

class (ParseResult result) => Extendable result where
  extend :: PartialResult Ast -> Until -> result

instance (Show ast) => ParseResult (PartialResult ast) where
  nextToken (PartialResult ast []) = Left (Abort NoTokens [])
  nextToken (PartialResult ast (x : xs)) = Right x
  tokens = unhandledTokens

instance (Show ast) => Extendable (PartialResult ast) where
  extend partialResult until = undefined

instance (Show ast) => ParseResult (Done ast) where
  nextToken = const $ Left (Abort NoTokens [])
  tokens = const []

instance ParseResult Abort where
  nextToken (Abort r []) = Left (Abort NoTokens [])
  nextToken (Abort r (x : xs)) = Right x
  tokens = leftoverTokens

class (Monad m, ParseResult r) => ParserMonad m r where
  nextToken' :: m r -> m (Either Abort Token)
  tokens' :: m r -> m [Token]

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
