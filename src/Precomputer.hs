module Precomputer where

import AST
import Interpreter
import Util.Parsing (Result (..))

precompute :: Env -> Expression -> Result String Expression
precompute env (List ((PreCompute expr) : xs)) = do
  expr' <- reduce env expr
  interpreted <- reduce env (List (expr' : map (Internal . Quote) xs))
  precompute env interpreted
precompute env (List other) = List <$> mapM (precompute env) other
precompute env (PreCompute expr) = Error $ "Precompute expression " ++ show expr ++ " precomputed directly."
precompute env other = Success other
