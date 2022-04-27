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

data Result a
  = Partial (PartialResult a)
  | Finished (Done a)
  | Aborted Abort

instance (Show a) => ParserMonad Result (PartialResult a) where
  nextToken' (Partial p) = return $ nextToken p
  nextToken' (Finished a) = return $ nextToken a
  nextToken' (Aborted a) = return $ nextToken a
  tokens' (Partial p) = return $ tokens p
  tokens' (Finished a) = return $ tokens a
  tokens' (Aborted a) = return $ tokens a

getA :: PartialResult a -> a
getA (PartialResult a tokens) = a

instance Monad Result where
  (>>=) (Aborted a) f = Aborted a
  (>>=) (Partial p) f = f $ getA p -- todo: incomplete
  (>>=) (Finished (Done a)) f = f a

instance Functor Result where
  fmap f ra = ra >>= return . f

instance Applicative Result where
  pure = return
  (<*>) mapr ra = do
    f <- mapr
    a <- ra
    pure $ f a

-- instance (ParseResult r) => ParserMonad (Result r) r

-- (>>=) p@(P) = undefined

{-

instance Monad PResult where
  return :: a -> PResult a
  return = Success
  (>>=) :: PResult a -> (a -> PResult b) -> PResult b
  (>>=) (Finished a) f = f a
  (>>=) (Part a tokens) f = f a
  (>>=) (Error r tokens) _ = Error r

instance Functor PResult where
  fmap f ra = ra >>= return . f

instance Applicative PResult where
  pure = return
  (<*>) mapr ra = do
    f <- mapr
    a <- ra
    pure $ f a

-}