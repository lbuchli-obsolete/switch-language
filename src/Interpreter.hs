{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Interpreter where

import AST
import Control.Monad (foldM, (>=>))
import Data.Bifunctor (bimap, first)
import Util.Parsing (Result (..), successOr)

reduce :: Env -> Expression -> Result String Expression
reduce env pre@(List ((PreCompute _) : _)) = precompute env pre
reduce env (List items) = do
  reduced <- mapM (reduce env) items
  -- reduced <- Success items -- Lazy evaluation
  Trace (show items) $ foldM (apply env) (head reduced) (tail reduced)
reduce env (Sym sym) = case lookup sym env of
  Just expr -> Success expr
  Nothing -> Error $ sym ++ " not in scope. Env: " ++ show env
reduce env (Internal (Quote expr)) = Success expr
reduce _ other = Success other

apply :: Env -> Expression -> Expression -> Result String Expression
apply _ (Internal Add) (Nbr x) = Success $ Internal $ Adding x
apply _ (Internal (Adding x)) (Nbr y) = Success $ Nbr $ x + y
apply _ (Internal Concat) (Str a) = Success . Internal . Concatenating $ a
apply _ (Internal (Concatenating a)) (Str b) = Success . Str $ a ++ b
apply _ (Internal Head) (List li) = Success $ head li
apply _ (Internal Tail) (List li) = Success . List $ tail li
apply _ (Internal Len) (List li) = Success . Nbr $ length li
apply _ (Internal Lambda0) (Sym sym) = Trace ("Lambda0 " ++ sym) $ Success . Internal $ Lambda1 sym
apply env (Internal (Lambda1 sym)) expr = Trace ("Lambda1 " ++ show expr) $ Success . Internal $ Lambda2 sym expr env
apply env (Internal (Lambda2 sym expr eenv)) val = Trace ("Lambda2 " ++ show val) $ reduce ((sym, val) : eenv) expr
apply env (Internal Equals0) v = Success . Internal $ Equals1 v
apply env (Internal (Equals1 v)) a = Success . Internal $ Equals2 v a
apply env (Internal (Equals2 v a)) b = Success . Internal $ Equals3 v a b
apply env (Internal (Equals3 v a b)) expr = reduce env $ if v == expr then a else b
apply env (Internal Let0) (Sym sym) = Success . Internal $ Let1 sym
apply env (Internal (Let1 sym)) val = Success . Internal $ Let2 sym val
apply env (Internal (Let2 sym val)) body = reduce ((sym, val) : env) body
apply env a@(List _) b = reduce env a >>= \a' -> apply env a' b
apply env a b@(List _) = reduce env b >>= \b' -> apply env a b'
-- apply env a b | not (remains env a) = reduce env a >>= \a' -> apply env a' b
-- apply env a b | not (remains env b) = reduce env b >>= \b' -> apply env a b'
apply _ a b = Error $ "Cannot apply " ++ show b ++ " to " ++ show a

precompute :: Env -> Expression -> Result String Expression
precompute env (List ((PreCompute expr) : xs)) = do
  expr' <- reduce env expr
  interpreted <- reduce env (List (expr' : map (Internal . Quote) xs))
  precompute env interpreted
precompute env (List other) = List <$> mapM (precompute env) other
precompute env (PreCompute expr) = Error $ "Precompute expression " ++ show expr ++ " precomputed directly."
precompute env other = Success other
