module Lib (run) where

import AST (Expression, prelude)
import Data.Bifunctor (bimap, first)
import Interpreter (reduce)
import Parser (parse)
import Precomputer (precompute)
import Util.Parsing (Result (..))

run :: String -> Result String Expression
run src = do
  parsed <- Trace "Parsing..." $ first (\(pos, msg) -> show pos ++ ": " ++ msg) (parse src)
  precomputed <- Trace "Precomputing..." $ precompute prelude parsed
  Trace "Interpreting..." $ reduce prelude precomputed
