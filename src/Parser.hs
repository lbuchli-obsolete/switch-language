{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import AST
import Control.Applicative (many, some, (<|>))
import Data.Functor (($>))
import Text.Read (readMaybe)
import Util.Parsing

parse :: String -> Result (Pos, String) Expression
parse src = (\(n, _, _) -> n) . snd <$> parseSrc (pList <* some ws <* eof) src

pExpression :: Parser String Error Expression
pExpression = pList <|> pPreCompute <|> pNbr <|> pCh <|> pStr <|> pSym

pPrecomputeExpression :: Parser String Error Expression
pPrecomputeExpression = pList <|> pNbr <|> pCh <|> pStr <|> pSym

pList :: Parser String Error Expression
pList =
  List <$> (str "(" *> sepBy (some ws) pExpression <* str ")")
    <|> str "()" $> List []

pPreCompute :: Parser String Error Expression
pPreCompute = PreCompute <$> (str ":" *> pPrecomputeExpression)

pNbr :: Parser String Error Expression
pNbr = Parser \i -> do
  num <- (\(n, _, _) -> n) . snd <$> parseSrc (some (anyOf "01234566789")) i
  case readMaybe num of
    Nothing -> parseError "Not an integer"
    Just x -> Success (drop (length num) i, (Nbr x, posOffsetOf num, num))

pCh :: Parser String Error Expression
pCh = Ch <$> (str "'" *> (noneOf "\'" <|> str "\\" *> noneOf "") <* str "'")

pStr :: Parser String Error Expression
pStr = Str <$> (str "\"" *> many (noneOf "\\\"" <|> str "\\" *> noneOf "") <* str "\"")

pSym :: Parser String Error Expression
pSym = Sym <$> some (noneOf "(): \t\n\\" <|> str "\\" *> noneOf " \t\n")
