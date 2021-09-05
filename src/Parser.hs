{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Applicative (many, some, (<|>))
import Data.Functor (($>))
import Text.Read (readMaybe)
import Util.Parsing

-- TODO implement type hints: expr: type
-- Type hints are just types (typevals) applied to expressions.

type Annotated a = (a, Type)

data Expression
  = Dict [(Annotated Expression, Annotated Expression)]
  | Appl [Annotated Expression]
  | Quote (Annotated Expression)
  | Unquote (Annotated Expression)
  | Nbr Int
  | Ch Char
  | Boolean Bool
  | ID String
  | TypeVal Type
  deriving (Eq, Show)

data Type
  = TDict [(Annotated Expression, Annotated Expression)]
  | TDictLen Int
  | TDictAny
  | TAppl [Annotated Expression]
  | TApplLen Int
  | TApplAny
  | TQuote (Annotated Expression)
  | TFn (Annotated Expression) (Annotated Expression)
  | TCh
  | TBool
  | TNbr
  | TID
  | TType
  | TAny
  deriving (Eq, Show)

parse :: String -> Result (Pos, String) (Annotated Expression)
parse src = (\(n, _, _) -> n) . snd <$> parseSrc (pDict <* some ws <* eof) src

pExpression :: Parser String Error (Annotated Expression)
pExpression = pDict <|> pAppl <|> pQuote <|> pUnquote <|> pNbr <|> pBoolean <|> pCh <|> pStr <|> pID

pDict :: Parser String Error (Annotated Expression)
pDict = (\x -> (Dict x, TDict $ replicate (length x) ((TypeVal TAny, TType), (TypeVal TAny, TType)))) <$> (str "[" *> many ws *> sepBy (some ws) p_dictentry <* many ws <* str "]")
  where
    p_dictentry = (,) <$> pExpression <*> (many ws *> str "=" *> many ws *> pExpression)

pAppl :: Parser String Error (Annotated Expression)
pAppl =
  (\x -> (Appl x, TAppl $ replicate (length x) (TypeVal TAny, TType))) <$> (str "(" *> sepBy (some ws) pExpression <* str ")")
    <|> str "()" $> (Appl [], TAppl [])

pTypeVal :: Parser String Error (Annotated Expression)
pTypeVal = undefined

pQuote :: Parser String Error (Annotated Expression)
pQuote = (\ae@(_, t) -> (Quote ae, TQuote (TypeVal t, TType))) <$> (str "#" *> pExpression)

pUnquote :: Parser String Error (Annotated Expression)
pUnquote = find_type <$> (str "*" *> pExpression)
  where
    find_type ae@(_, TQuote (TypeVal t, _)) = (Unquote ae, t)
    find_type (e, _) = (Unquote (e, TQuote (TypeVal TAny, TType)), TAny)

pNbr :: Parser String Error (Annotated Expression)
pNbr = Parser \i -> do
  num <- (\(n, _, _) -> n) . snd <$> parseSrc (some (anyOf "01234566789")) i
  case readMaybe num of
    Nothing -> parseError "Not an integer"
    Just x -> Success (drop (length num) i, ((Nbr x, TNbr), posOffsetOf num, num))

pCh :: Parser String Error (Annotated Expression)
pCh = (\x -> (Ch x, TCh)) <$> (str "'" *> (noneOf "\'" <|> str "\\" *> noneOf "") <* str "'")

pBoolean :: Parser String Error (Annotated Expression)
pBoolean =
  ((Boolean True, TBool) <$ str "true")
    <|> ((Boolean False, TBool) <$ str "false")

pStr :: Parser String Error (Annotated Expression)
pStr =
  (\x@(_, t) -> (Quote x, TQuote (TypeVal t, TType)))
    . (\x -> (Appl x, TAppl $ replicate (length x) (TypeVal TCh, TType)))
    . map (\x -> (Ch x, TCh))
    <$> (str "\"" *> many (noneOf "\\\"" <|> str "\\" *> noneOf "") <* str "\"")

pID :: Parser String Error (Annotated Expression)
pID = (\x -> (ID x, TID)) <$> some (anyOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_")
