{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Util.Parsing where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

----------------------------------------------------------------------
--                              Result                              --
----------------------------------------------------------------------

data Result e a = Success a | Trace String (Result e a) | Error e

instance Functor (Result e) where
  fmap f (Success a) = Success (f a)
  fmap f (Trace msg r) = Trace msg $ fmap f r
  fmap _ (Error msg) = Error msg

instance Bifunctor Result where
  bimap _ fa (Success a) = Success (fa a)
  bimap fe fa (Trace msg r) = Trace msg $ bimap fe fa r
  bimap fe _ (Error msg) = Error (fe msg)

instance Applicative (Result e) where
  pure x = Success x
  (<*>) (Success f) (Success a) = Success (f a)
  (<*>) (Trace _ rf) ra = rf <*> ra
  (<*>) (Error msg) _ = Error msg
  (<*>) rf (Trace _ ra) = rf <*> ra
  (<*>) _ (Error msg) = Error msg

instance Monad (Result e) where
  (>>=) (Success a) f = f a
  (>>=) (Trace msg a) f = Trace msg (a >>= f)
  (>>=) (Error msg) _ = Error msg

instance Alternative (Result e) where
  empty = Error (error "Error")
  (<|>) (Trace m a) b = Trace m $ a <|> b
  (<|>) (Success a) _ = Success a
  (<|>) (Error a) (Trace m b) = Trace m $ Error a <|> b
  (<|>) (Error _) (Success b) = Success b
  (<|>) (Error a) (Error _) = Error a

instance Eq a => Eq (Result e a) where
  (==) (Success a) (Success b) = a == b
  (==) (Success _) _ = False
  (==) (Trace msg_a a) (Trace msg_b b) = msg_a == msg_b && a == b
  (==) (Trace _ _) _ = False
  (==) (Error _) (Error _) = True
  (==) (Error _) _ = False

instance (Show e, Show a) => Show (Result e a) where
  show (Success a) = "Success " ++ show a
  show (Trace msg a) = "Trace '" ++ msg ++ "'\n" ++ show a
  show (Error e) = "Error: " ++ show e

type ShowTrace = Bool

printResult :: (Show e, Show a) => ShowTrace -> Result e a -> IO ()
printResult _ (Error e) = print e
printResult _ (Success s) = print s
printResult True (Trace msg r) = do
  putStr "TRACE: "
  putStrLn msg
  printResult True r
printResult False (Trace _ r) = printResult False r

noTrace :: Result e a -> Result e a
noTrace (Success a) = Success a
noTrace (Trace _ a) = noTrace a
noTrace (Error e) = Error e

successOr :: Result e a -> a -> a
successOr (Success a) _ = a
successOr (Trace _ x) a = successOr x a
successOr (Error _) a = a

----------------------------------------------------------------------
--                              Parser                              --
----------------------------------------------------------------------

-- |
-- A position in the source code.
data Pos = Pos
  { row :: Int,
    col :: Int
  }
  deriving (Eq, Show)

instance Ord Pos where
  compare a b = if by_row == EQ then by_col else by_row
    where
      by_row = compare (row a) (row b)
      by_col = compare (col a) (col b)

addPos :: Pos -> Pos -> Pos
addPos (Pos x y) (Pos x' y') = Pos (x + x') (y + y')

posOffsetOf :: String -> Pos
posOffsetOf s =
  Pos
    (length $ filter (== '\n') s)
    (Data.Maybe.fromMaybe (length s) $ elemIndex '\n' $ reverse s)

-- |
-- Parser; Parses source code and spits out some structured representation of it
-- or an error if parsing the source wasn't possible
newtype Parser i e a = Parser
  { parseSrc :: i -> Result (Pos, e) (i, (a, Pos, String))
  }

annotate :: (a -> Pos -> String -> b) -> Parser i e a -> Parser i e b
annotate f b =
  Parser $ \i -> fmap (\(i', (a, p, s)) -> (i', (f a p s, p, s))) (parseSrc b i)

instance Functor (Parser i e) where
  fmap f b =
    Parser $ \i -> fmap (\(i', (a, p, s)) -> (i', (f a, p, s))) (parseSrc b i)

instance Applicative (Parser i e) where
  pure x = Parser $ \i -> pure (i, (x, Pos 0 0, ""))
  (<*>) f g = Parser $ parse_g <=< parseSrc f
    where
      parse_g (i', (func, pf, tf)) =
        bimap
          (first (addPos pf))
          (\(i'', (a, pg, tg)) -> (i'', (func a, addPos pf pg, tf ++ tg)))
          (parseSrc g i')

instance Alternative (Parser i e) where
  empty = Parser $ const empty
  (<|>) pa pb = Parser $ \i -> pick (parseSrc pa i) (parseSrc pb i)
    where
      pick (Trace m a) b = Trace m $ pick a b
      pick (Success a) _ = Success a
      pick (Error a) (Trace m b) = Trace m $ pick (Error a) b
      pick (Error _) (Success b) = Success b
      pick a@(Error (pos_a, _)) b@(Error (pos_b, _)) =
        if pos_a < pos_b then b else a -- choose the one that came the farthest

instance Monad (Parser i e) where
  (>>=) pa pb =
    Parser $
      parseSrc pa >=> \(i', (a, p, t)) ->
        fmap
          (\(i'', (b, p', t')) -> (i'', (b, addPos p p', t ++ t')))
          (parseSrc (pb a) i')

---------------------------------------------------------------------------
--                         Parser Implementations                        --
---------------------------------------------------------------------------

type Error = String

shorten :: String -> String
shorten s = take 16 s ++ if length s > 16 then "..." else ""

sepBy ::
  Parser String Error a -> Parser String Error b -> Parser String Error [b]
sepBy a b = (:) <$> b <*> many (a *> b)

ws :: Parser String Error ()
ws = void (anyOf "\t \n")

str :: String -> Parser String Error String
str s = Parser $ \i ->
  if take len i == s
    then Success (drop len i, (s, posOffsetOf s, s))
    else parseError ("Input does not match '" ++ s ++ "': " ++ shorten i)
  where
    len = length s

noneOf :: [Char] -> Parser String Error Char
noneOf vs = Parser $ \case
  [] -> parseError "Empty input"
  (x : _) | elem x vs -> parseError $ "Should not match '" ++ [x] ++ "'"
  (x : xs) -> Success (xs, (x, posOffsetOf [x], [x]))

anyOf :: [Char] -> Parser String Error Char
anyOf vs = Parser $ \case
  [] -> parseError "Empty input"
  (x : xs) | elem x vs -> Success (xs, (x, posOffsetOf [x], [x]))
  (_ : _) -> parseError $ "Input does not match any of '" ++ vs ++ "'"

count :: Parser String Error a -> Parser String Error Int
count p = length <$> many p

eof :: Parser String Error ()
eof = Parser $ \i ->
  if null i
    then Success ("", ((), Pos 0 0, ""))
    else Error (Pos 0 0, "There is still input left: " ++ shorten i)

pEmpty :: Parser i e ()
pEmpty = Parser $ \i -> Success (i, ((), Pos 0 0, ""))

parseError :: String -> Result (Pos, String) a
parseError s = Error (Pos 0 0, s)

pFail :: String -> Parser i Error a
pFail msg = Parser $ \_ -> parseError msg

(<?>) :: Parser i Error a -> Error -> Parser i Error a
(<?>) p m = pFail m <|> p
