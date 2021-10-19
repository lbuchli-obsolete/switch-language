{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Interpreter where

import AST
import Control.Monad (foldM, (>=>))
import Data.Bifunctor (bimap, first)
import Util.Parsing (Result (..), successOr)

reduce :: Env -> Expression -> Result String Expression
reduce env (List items) = do
  reduced <- mapM (reduce env) items
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
apply _ (Internal Tail) (List li) = Success $ List $ tail li
apply _ (Internal Len) (List li) = Success $ Nbr $ length li
apply _ (Internal Lambda0) (Sym sym) = Success $ Internal $ Lambda1 sym
apply env (Internal (Lambda1 sym)) expr = Success $ Internal $ Lambda2 sym expr env
apply env (Internal (Lambda2 sym expr eenv)) val = reduce ((sym, val) : eenv) expr
apply env a@(List _) b = reduce env a >>= \a' -> apply env a' b
apply env a b@(List _) = reduce env b >>= \b' -> apply env a b'
apply _ a b = Error $ "Cannot apply " ++ show b ++ " to " ++ show a
