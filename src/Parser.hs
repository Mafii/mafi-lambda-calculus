module Parser (parse) where

import Data.Either (fromLeft)
import qualified Data.Foldable
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Lib (Term)
import qualified Lib
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

data TokenOrScope
  = T Token
  | Scope [TokenOrScope]
  deriving (Show)

data IntermediateParseResult
  = Var String
  | App IntermediateParseResult (Either IntermediateParseResult CompletedParseResult)
  | AppPC CompletedParseResult IntermediateParseResult -- lhs is fully completed scope but rhs is extendable
  deriving (Show)

data CompletedParseResult
  = VarC String
  | AbsC String CompletedParseResult
  | AppC CompletedParseResult CompletedParseResult
  deriving (Show)

data Extendable = E IntermediateParseResult [TokenOrScope] | Closed CompletedParseResult [TokenOrScope]
  deriving (Show)

parse :: [Tokenizer.Token] -> Term
parse tokens =
  toTerm $
    parse' $
      removeEmptyScopes $
        getScopes $
          flatMapTokens tokens

removeEmptyScopes :: [TokenOrScope] -> [TokenOrScope]
removeEmptyScopes = concatMap (Data.Foldable.toList . f)
  where
    f (T el) = Just $ T el
    f (Scope []) = Nothing
    f (Scope s) = Just $ Scope $ removeEmptyScopes s

-- >>> parse $ Tokenizer.tokenize "(a(b)c)"
-- ((a b) c)

parse' :: [TokenOrScope] -> CompletedParseResult
parse' inp | trace ("parse': " ++ show inp) False = undefined
parse' tokens = ensureCompleted $ until isCompletedOrError extend (getFirstElement tokens)

-- extend

extend :: Extendable -> Extendable
extend (E element []) = error "can't extend when tokens are missing"
extend (E element unhandled) = extendIm element unhandled
extend (Closed element []) = error "can't extend when tokens are missing"
extend (Closed element unhandled) = extendComp element unhandled

extendIm :: IntermediateParseResult -> [TokenOrScope] -> Extendable
extendIm (AppPC lhs rhs) tokens = case extendIm rhs tokens of
  E intermediate rest -> E (AppPC lhs intermediate) rest
  Closed completed rest -> Closed (AppC lhs completed) rest
extendIm value (Scope scopeTokens : tokens) = do
  let newRhs = parse' scopeTokens
  Closed (AppC (toCompleted value) newRhs) tokens
extendIm value tokens = do
  let (rhs, rest) = case getFirstElement tokens of
        E rhs rest -> (Left rhs, rest)
        Closed rhs rest -> (Right rhs, rest)
  case rest of
    [] -> Closed (AppC (toCompleted value) (toCompleted' rhs)) []
    tokens -> E (App value rhs) tokens

extendComp :: CompletedParseResult -> [TokenOrScope] -> Extendable
extendComp completed tokens = do
  let next = getFirstElement tokens
  applyToCompleted completed next

applyToCompleted :: CompletedParseResult -> Extendable -> Extendable
applyToCompleted lhs (E el []) = Closed (AppC lhs (toCompleted el)) []
applyToCompleted lhs (Closed el tks) = Closed (AppC lhs el) tks
applyToCompleted lhs (E el tks) = E (AppPC lhs el) tks

getFirstElement :: [TokenOrScope] -> Extendable
getFirstElement x | trace ("getFirstElement: " ++ show x) False = undefined
getFirstElement ((Scope inner) : tokens) = do
  let scopeContent = parse' inner
  let extendable = Closed scopeContent tokens
  let traced = trace ("getFirstTrace: " ++ show extendable) extendable
  traced
getFirstElement (T Lambda : (T (VarUseOrBind id)) : T Dot : tokens) = do
  let body = parse' tokens
  Closed (AbsC id body) tokens
getFirstElement (T (VarUseOrBind id) : s@Scope {} : tokens) = Closed (VarC id) (s : tokens)
getFirstElement (T (VarUseOrBind id) : tokens) = E (Var id) tokens
getFirstElement val = error $ "Unhandled (possibly invalid) sequence: " ++ show val

unpackClose :: Extendable -> CompletedParseResult
unpackClose (E el []) = toCompleted el
unpackClose (Closed el []) = el
unpackClose val = error $ "unexpected usage of unfinished result: " ++ show val

-- scope

getScopes :: [Token] -> [TokenOrScope]
getScopes tokens =
  ensureFullResult $
    until
      isCompleted
      getScope
      $ Aggregating
        (ExpandableScope [] 0)
        tokens
        0

isCompleted :: Scope -> Bool
isCompleted el | trace (show el) False = undefined
isCompleted (Aggregating _ [] 0) = True
isCompleted val = trace (show val) False

getScope :: Scope -> Scope
getScope el | trace (show el) False = undefined
getScope finished@(Aggregating handled [] 0) = finished
getScope (Aggregating handled [ClosingParens] depth) = Aggregating handled [] (depth - 1)
getScope (Aggregating handled (ClosingParens : tokens) depth) = Aggregating handled tokens (depth - 1)
getScope (Aggregating handled (OpenParens : tokens) depth) = Aggregating (openScope handled depth) tokens (depth + 1)
getScope (Aggregating handled (next : tokens) depth) = Aggregating (expandOrAppend handled next depth) tokens depth
getScope finished@(Aggregating handled [] n) = error "missing closing parenthesis"

expandOrAppend :: HandledTokens -> Token -> BracketDepth -> HandledTokens
expandOrAppend h t d | trace ("eoa: " ++ show h ++ show t ++ show d) False = undefined
expandOrAppend _ ClosingParens _ = error "mistake"
expandOrAppend _ OpenParens _ = error "mistake"
expandOrAppend (ExpandableScope els depth) element currentDepth = do
  if depth == currentDepth
    then ExpandableScope (els ++ [Tk element]) currentDepth
    else impl els element currentDepth
  where
    impl els = impl' (reverse els)
    impl' origs@((Sc (ExpandableScope els depth)) : rest) element currentDepth =
      if depth == currentDepth
        then ExpandableScope (reverse rest ++ [Sc $ ExpandableScope (els ++ [Tk element]) depth]) currentDepth
        else ExpandableScope (reverse origs ++ [Tk element]) currentDepth
    impl' origs element currentDepth = ExpandableScope (reverse origs ++ [Tk element]) currentDepth
expandOrAppend _ _ _ = error "unexpected"

openScope :: HandledTokens -> BracketDepth -> HandledTokens
openScope (ExpandableScope els outer) depth = ExpandableScope (els ++ [Sc $ ExpandableScope [] (depth + 1)]) outer
openScope _ _ = error "unexpected"

-- manual scope grabber test

-- >>>

-- >>> getScopes' ([OpenParens, OpenParens, VarUseOrBind "a", ClosingParens, ClosingParens])
-- Aggregating (ExpandableScope [Sc (ExpandableScope [] 1),Sc (ExpandableScope [Tk (VarUseOrBind "a")] 2)] 2) [] 0

-- >>> getScopes $ flatMapTokens $ Tokenizer.tokenize "(a(b)c)"
-- bug, not finished: Aggregating (ExpandableScope [Sc (ExpandableScope [] 1)] 0) [OpenParens,VarUseOrBind "a",OpenParens,VarUseOrBind "b",ClosingParens,VarUseOrBind "c",ClosingParens,ClosingParens] 1

-- >>> getScopes $ flatMapTokens $ Tokenizer.tokenize "a (b c)"
-- [T (VarUseOrBind "a"),Scope [T (VarUseOrBind "b")],T (VarUseOrBind "c")]

type NewTokens = [Token]

type HandledTokens = ExpandableScope

data AggrTokenOrScope
  = Tk Token
  | Sc ExpandableScope
  deriving (Show)

data ExpandableScope = ExpandableScope [AggrTokenOrScope] BracketDepth | CompletedScope [AggrTokenOrScope]
  deriving (Show)

type BracketDepth = Int

data Scope = Aggregating HandledTokens NewTokens BracketDepth
  deriving (Show)

ensureFullResult :: Scope -> [TokenOrScope]
ensureFullResult val | trace "ensurefullresult" False = undefined
ensureFullResult (Aggregating (CompletedScope els) [] 0) = map toToS els
ensureFullResult (Aggregating (ExpandableScope els _) [] 0) = map toToS els
ensureFullResult val = error $ "bug, not finished: " ++ show val

toToS :: AggrTokenOrScope -> TokenOrScope
toToS (Tk t) = T t
toToS (Sc (CompletedScope els)) = Scope $ map toToS els
toToS (Sc (ExpandableScope els d)) = Scope $ map toToS els

-- helpers

toTerm :: CompletedParseResult -> Term
toTerm (AppC l r) = Lib.App (toTerm l) (toTerm r)
toTerm (AbsC id r) = Lib.Abs id (toTerm r)
toTerm (VarC id) = Lib.Var id

toCompleted :: IntermediateParseResult -> CompletedParseResult
toCompleted (App l r) = AppC (toCompleted l) (toCompleted' r)
toCompleted (Var id) = VarC id
toCompleted (AppPC l r) = AppC l (toCompleted r)

toCompleted' :: Either IntermediateParseResult CompletedParseResult -> CompletedParseResult
toCompleted' (Right val) = val
toCompleted' (Left val) = toCompleted val

ensureCompleted :: Extendable -> CompletedParseResult
ensureCompleted (E el []) = toCompleted el
ensureCompleted (Closed el []) = el
ensureCompleted val = error $ "incomplete parse result: " ++ show val

isCompletedOrError :: Extendable -> Bool
-- isCompletedOrError (Extender a b) | trace ("check completed: " ++ show a) False = undefined
isCompletedOrError ex = do
  let check = isCompletedOrError' ex
  let checkt = trace ("isCompl " ++ show ex ++ " ?: " ++ show check) check
  checkt

isCompletedOrError' :: Extendable -> Bool
isCompletedOrError' (E e []) = True
isCompletedOrError' (Closed e []) = True
isCompletedOrError' _ = False