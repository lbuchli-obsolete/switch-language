{-# LANGUAGE TupleSections #-}

module TypeChecker where

import Data.Bifunctor (bimap)
import qualified Parser as P
import Util.Parsing (Result (..))

data Expression
  = Dict [(Expression, Expression)]
  | Appl [Expression]
  | Quote Expression
  | Unquote Expression
  | Nbr Int
  | Ch Char
  | Boolean Bool
  | ID String
  | TypeVal Type
  | Internal Internal
  deriving (Eq, Show)

data Internal
  = Add
  | Adding Int
  | Concat
  | Concatenating String
  | Head
  | Tail
  | Len
  | Lambda0
  | Lambda1 String
  | Lambda2 String Expression [(String, Expression)]
  | ElemOf0
  | ElemOf1 [(Expression, Expression)]
  deriving (Eq, Show)

data Type
  = TDict [(Expression, Expression)]
  | TDictLen Int
  | TDictAny
  | TAppl [Expression]
  | TApplLen Int
  | TApplAny
  | TQuote Expression
  | TFn Expression Expression
  | TCh
  | TBool
  | TNbr
  | TID
  | TType
  | TAny
  deriving (Eq, Show)

check :: P.Annotated P.Expression -> Result String Expression
check (P.Dict xs, _) = Dict <$> mapM (transpose . bimap check check) xs
check (P.Appl xs, _) = Appl <$> mapM check xs
check (P.Quote x, _) = Quote <$> check x
check (P.Unquote x, _) = Unquote <$> check x
check (P.Nbr i, _) = Success $ Nbr i
check (P.Ch c, _) = Success $ Ch c
check (P.Boolean b, _) = Success $ Boolean b
check (P.ID id, _) = Success $ ID id
check (P.TypeVal t, _) = TypeVal <$> checkType t

checkType :: P.Type -> Result String Type
checkType (P.TDict xs) = TDict <$> mapM (transpose . bimap check check) xs
checkType (P.TDictLen i) = Success $ TDictLen i
checkType P.TDictAny = Success TDictAny
checkType (P.TAppl xs) = TAppl <$> mapM check xs
checkType (P.TApplLen i) = Success $ TApplLen i
checkType P.TApplAny = Success TApplAny
checkType (P.TQuote x) = TQuote <$> check x
checkType (P.TFn a b) = do
  a' <- check a
  b' <- check b
  return (TFn a' b')
checkType P.TCh = Success TCh
checkType P.TBool = Success TBool
checkType P.TNbr = Success TNbr
checkType P.TID = Success TID
checkType P.TType = Success TType
checkType P.TAny = Success TAny

transpose :: Monad m => (m a, m b) -> m (a, b)
transpose (a, b) = do
  a' <- a
  b' <- b
  return (a', b')
