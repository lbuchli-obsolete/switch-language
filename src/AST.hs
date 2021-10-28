module AST where

import Util.Parsing (Result (..), successOr)

type Env = [(String, Expression)]

type Symbol = String

data Expression
  = List [Expression]
  | PreCompute Expression -- This should not be around when interpreting
  | Nbr Int
  | Ch Char
  | Str String
  | Sym Symbol
  | Internal Internal
  deriving (Eq, Show)

data Internal
  = Add
  | Adding Int
  | Concat
  | Concatenating String
  | Head
  | Tail
  | Len
  | Lambda0
  | Lambda1 String
  | Lambda2 String Expression [(String, Expression)]
  | Equals0
  | Equals1 Expression -- value to compare
  | Equals2 Expression Expression -- branch a
  | Equals3 Expression Expression Expression -- branch b
  | Let0
  | Let1 String -- name
  | Let2 String Expression -- value
  | Quote Expression
  deriving (Eq, Show)

prelude :: [(String, Expression)]
prelude =
  [ ("add", Internal Add),
    ("concat", Internal Add),
    ("head", Internal Head),
    ("tail", Internal Tail),
    ("len", Internal Len),
    ("lambda", Internal Lambda0),
    ("equals", Internal Equals0),
    ("let", Internal Let0)
  ]
