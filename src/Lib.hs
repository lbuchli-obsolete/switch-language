module Lib (run) where

import AST (Expression)
import Data.Bifunctor (bimap, first)
import Interpreter (interpret)
import Parser (parse)
import TypeChecker (check)
import Util.Parsing (Result (..))

run :: String -> Result String Expression
run src = do
  parsed <- first (\(pos, msg) -> show pos ++ ": " ++ msg) (parse src)
  checked <- check parsed
  interpret checked
