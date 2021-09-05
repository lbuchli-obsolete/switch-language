{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import AST
import Control.Applicative (many, some, (<|>))
import Data.Functor (($>))
import Text.Read (readMaybe)
import Util.Parsing

parse :: String -> Result (Pos, String) Expression
parse src = (\(n, _, _) -> n) . snd <$> parseSrc (pDict <* some ws <* eof) src

pExpression :: Parser String Error Expression
pExpression = pDict <|> pAppl <|> pQuote <|> pUnquote <|> pPreCompute <|> pNbr <|> pCh <|> pStr <|> pID

pDict :: Parser String Error Expression
pDict = Dict <$> (str "[" *> many ws *> sepBy (some ws) p_dictentry <* many ws <* str "]")
  where
    p_dictentry = (,) <$> pExpression <*> (many ws *> str "=" *> many ws *> pExpression)

pAppl :: Parser String Error Expression
pAppl =
  Appl <$> (str "(" *> sepBy (some ws) pExpression <* str ")")
    <|> str "()" $> Appl []

pQuote :: Parser String Error Expression
pQuote = Quote <$> (str "#" *> pExpression)

pUnquote :: Parser String Error Expression
pUnquote = Unquote <$> (str "*" *> pExpression)

pPreCompute :: Parser String Error Expression
pPreCompute = PreCompute <$> (str "@" *> pExpression)

pNbr :: Parser String Error Expression
pNbr = Parser \i -> do
  num <- (\(n, _, _) -> n) . snd <$> parseSrc (some (anyOf "01234566789")) i
  case readMaybe num of
    Nothing -> parseError "Not an integer"
    Just x -> Success (drop (length num) i, (Nbr x, posOffsetOf num, num))

pCh :: Parser String Error Expression
pCh = Ch <$> (str "'" *> (noneOf "\'" <|> str "\\" *> noneOf "") <* str "'")

pStr :: Parser String Error Expression
pStr = Quote . Appl . map Ch <$> (str "\"" *> many (noneOf "\\\"" <|> str "\\" *> noneOf "") <* str "\"")

pID :: Parser String Error Expression
pID = ID <$> some (noneOf "[]()#* \t\n\\" <|> str "\\" *> noneOf " \t\n")
