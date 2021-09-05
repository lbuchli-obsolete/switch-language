{-# LANGUAGE TupleSections #-}

module TypeChecker where

import Control.Monad ((>=>))
import Data.Bifunctor (bimap)
import Interpreter (interpret)
import qualified Parser as P
import RuntimeAST
import Util.Parsing (Result (..))

type Env = [(String, P.Annotated P.Expression)]

check :: P.Annotated P.Expression -> Result String Expression
check = checkExpr [] P.TAny >=> Success . fst

checkExpr ::
  -- | Runtime Environment
  Env ->
  -- | Type Restriction
  Type ->
  -- | Parsed Expression & Type
  P.Annotated P.Expression ->
  -- | Resulting Expression and inferred type
  Result String (Expression, Type)
checkExpr env t (P.Dict xs, _) = Dict <$> mapM (transpose . bimap check check) xs
checkExpr env t (P.Appl xs, _) = Appl <$> mapM check xs
checkExpr env t (P.Quote x, _) = Quote <$> check env x
checkExpr env t (P.Unquote x, _) = Unquote <$> check env x
checkExpr env t (P.Nbr i, _) = Success (Nbr i, TNbr)
checkExpr env t (P.Ch c, _) = Success $ Ch c
checkExpr env t (P.ID id, _) = Success $ ID id
checkExpr env t (P.TypeVal x, _) = TypeVal <$> checkType env t
checkExpr env t (P.PreCompute expr, _) = (check env >=> interpret) expr

checkType :: Env -> P.Type -> Result String (Type, Type)
checkType env (P.TDict xs) = TDict <$> mapM (transpose . bimap check check) xs
checkType env (P.TDictLen i) = Success $ TDictLen i
checkType env P.TDictAny = Success TDictAny
checkType env (P.TAppl xs) = TAppl <$> mapM check xs
checkType env (P.TApplLen i) = Success $ TApplLen i
checkType env P.TApplAny = Success TApplAny
checkType env (P.TQuote x) = TQuote <$> check env x
checkType env (P.TFn a b) = do
  (a', ta) <- check env TType a
  (b', tb) <- check env TType b
  return (TFn a' b', TType)
checkType _ P.TCh = Success TCh
checkType _ P.TNbr = Success TNbr
checkType _ P.TID = Success TID
checkType _ P.TType = Success TType
checkType _ P.TAny = Success TAny

combine :: Type -> Type -> Result String Type
combine = undefined

transpose :: Monad m => (m a, m b) -> m (a, b)
transpose (a, b) = do
  a' <- a
  b' <- b
  return (a', b')
