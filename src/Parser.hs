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
  List <$> (str "(" *> many ws *> sepBy (some ws) pExpression <* many ws <* str ")")
    <|> str "()" $> List []

pPreCompute :: Parser String Error Expression
pPreCompute = PreCompute <$> (str ":" *> pPrecomputeExpression)

pNbr :: Parser String Error Expression
pNbr = Nbr <$> (readNum =<< nbr)
  where
    nbr = (++) <$> (str "-" <|> ("" <$ pEmpty)) <*> some (anyOf "0123456789")
    readNum n = case readMaybe n of
      Nothing -> pFail "Not an integer"
      Just x -> return x

pCh :: Parser String Error Expression
pCh = Ch <$> (str "'" *> (noneOf "\'" <|> str "\\" *> noneOf "") <* str "'")

pStr :: Parser String Error Expression
pStr = Str <$> (str "\"" *> many (noneOf "\\\"" <|> str "\\" *> noneOf "") <* str "\"")

pSym :: Parser String Error Expression
pSym = Sym <$> some (noneOf "(): \t\n\\" <|> str "\\" *> noneOf " \t\n")
