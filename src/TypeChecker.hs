{-# LANGUAGE TupleSections #-}

module TypeChecker where

import AST
import Control.Monad ((>=>))
import Data.Bifunctor (bimap, first)
import Interpreter (reduce)
import Util.Parsing (Result (..))

check :: Expression -> Result String Expression
check = checkExpr prelude TAny >=> Success . fst

checkExpr ::
  -- | Runtime Environment
  Env ->
  -- | Type Restriction
  Type ->
  -- | Parsed Expressione
  Expression ->
  -- | Resulting Expression and inferred type
  Result String (Expression, Type)
checkExpr env t (Dict xs) = do
  -- TODO combine t
  (ks, kts) <- unzip <$> mapM (checkExpr env TAny . fst) xs
  (vs, vts) <- unzip <$> mapM (checkExpr (levelenv xs ++ env) TAny . snd) xs
  -- TODO build levelenv from checked expr
  -- 1. Manipulate env so current expression contains Internal SelfReference with name
  -- 2. When checking Internal SelfReference, turn it back to ID with no type checking
  return (Dict (zip ks vs), TDict (zip (map TypeVal kts) (map TypeVal vts)))
checkExpr env t (Appl xs) = do
  -- TODO combine t
  -- TODO type hints
  (xs', ts) <- unzip <$> mapM (checkExpr env TAny) xs
  return (Appl xs', TAppl $ map TypeVal ts)
checkExpr env t (Quote x) = first Quote <$> checkExpr env TAny x -- TODO combine t
checkExpr env t (Unquote x) = first Unquote <$> checkExpr env TAny x -- TODO combine t
checkExpr env t (Nbr i) = Success (Nbr i, TNbr) -- TODO combine t
checkExpr env t (Ch c) = Success (Ch c, TCh) -- TODO combine t
checkExpr env t (ID id) = Success (ID id, TID id) -- TODO combine t
checkExpr env t (PreCompute expr) = checkExpr env TAny =<< (checkExpr env TAny >=> reduce env . fst) expr -- TODO combine t
checkExpr env t (TypeVal x) = (,TType) . TypeVal <$> checkType env t -- TODO combine t
checkExpr env t other = Success (other, TAny) -- TODO

checkType :: Env -> Type -> Result String Type
checkType env (TDict xs) = do
  (ks, kts) <- unzip <$> mapM (checkExpr env TAny . fst) xs
  (vs, vts) <- unzip <$> mapM (checkExpr (levelenv xs ++ env) TAny . snd) xs
  return (TDict (zip ks vs))
checkType env (TDictLen i) = Success $ TDictLen i
checkType env TDictAny = Success TDictAny
checkType env (TAppl xs) = TAppl <$> mapM check xs
checkType env (TApplLen i) = Success $ TApplLen i
checkType env TApplAny = Success TApplAny
checkType env (TUnion ts) = TUnion <$> mapM check ts
checkType env (TQuote x) = TQuote . fst <$> checkExpr env TType x
checkType env (TFn a b) = do
  (a', ta) <- checkExpr env TType a
  (b', tb) <- checkExpr env TType b
  return (TFn a' b')
checkType _ TCh = Success TCh
checkType _ TNbr = Success TNbr
checkType _ (TID id) = Success (TID id)
checkType _ TIDAny = Success TIDAny
checkType _ TType = Success TType
checkType _ TAny = Success TAny

combine :: Env -> Expression -> Expression -> Result String Expression
combine env x (TypeVal TAny) = Success x
combine env (TypeVal (TDict xs)) (TypeVal (TDict ys)) = undefined
combine env (TypeVal (TDict xs)) (TypeVal (TDictLen _)) = Success $ TypeVal $ TDict xs
combine env (TypeVal (TDict xs)) (TypeVal TDictAny) = Success $ TypeVal $ TDict xs
combine env a@(TypeVal (TDict _)) b = nomatch a b
combine env a@(TypeVal (TDictLen _)) b@(TypeVal (TDictLen _)) | a == b = Success a
combine env a@(TypeVal (TDictLen _)) b@(TypeVal (TDictLen _)) | a /= b = nomatch a b
combine env a@(TypeVal (TDictLen _)) (TypeVal TDictAny) = Success a
combine env a@(TypeVal (TDictLen _)) b = nomatch a b
combine env a@(TypeVal TDictAny) (TypeVal TDictAny) = Success a
combine env a@(TypeVal TDictAny) b = nomatch a b
-- TODO TAppl, TUnion
combine env (TypeVal (TQuote a)) (TypeVal (TQuote b)) = TypeVal . TQuote <$> combine env a b
combine env a@(TypeVal (TQuote _)) b = nomatch a b
combine env (TypeVal (TFn a b)) (TypeVal (TFn a' b')) = TypeVal <$> (TFn <$> combine env a a' <*> combine env b b')
combine env a@(TypeVal (TFn _ _)) b = nomatch a b
combine env (TypeVal a) (TypeVal b) | a == b = Success $ TypeVal a
combine env a@(TypeVal _) b = nomatch a b
combine env a b@(TypeVal _) = combine env b a
combine env a b = undefined -- TODO evaluate argument

nomatch :: Expression -> Expression -> Result String a
nomatch a b = Error $ "Cannot match types " ++ show a ++ " and " ++ show b

transpose :: Monad m => (m a, m b) -> m (a, b)
transpose (a, b) = do
  a' <- a
  b' <- b
  return (a', b')
