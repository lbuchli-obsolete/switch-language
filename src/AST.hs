module AST where

import Util.Parsing (Result (..), successOr)

type Env = [(String, Expression)]

data Expression
  = Dict [(Expression, Expression)]
  | Appl [Expression]
  | Quote Expression
  | Unquote Expression
  | PreCompute Expression -- This should not be around when interpreting
  | TypeVal Type
  | Nbr Int
  | Ch Char
  | ID String
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
  | ElemOf0
  | ElemOf1 [(Expression, Expression)]
  | ITDict
  | ITDictLen
  | ITAppl
  | ITApplLen
  | ITUnion
  | ITQuote
  | ITFn0
  | ITFn1 Type
  | ITCh
  | ITNbr
  | ITID
  | ITType
  | ITAny
  | SelfReference String
  deriving (Eq, Show)

data Type
  = TDict [(Expression, Expression)] -- TODO no dicts
  | TDictLen Int
  | TDictAny
  | TAppl [Expression]
  | TApplLen Int -- TODO remove can be implemented by user
  | TApplAny
  | TUnion [Expression]
  | TQuote Expression
  | TFn Expression Expression
  | TCh
  | TNbr
  | TID String -- TODO is e.g. 3 a type of itself? if so, this is unnessessary
  | TIDAny
  | TType
  | TAny
  deriving (Eq, Show)

prelude :: [(String, Expression)]
prelude =
  [ ("add", Internal Add),
    ("concat", Internal Add),
    ("head", Internal Head),
    ("tail", Internal Tail),
    ("len", Internal Len),
    ("lambda", Internal Lambda0),
    ("elemof", Internal ElemOf0),
    ("Dict", Internal ITDict),
    ("DictLen", Internal ITDictLen),
    ("DictAny", TypeVal TDictAny),
    ("List", Internal ITAppl),
    ("ListLen", Internal ITApplLen),
    ("ListAny", TypeVal TApplAny),
    ("Union", Internal ITUnion),
    ("Quote", Internal ITQuote),
    ("Fn", Internal ITFn0),
    ("Chr", TypeVal TCh),
    ("Nbr", TypeVal TNbr),
    ("ID", Internal ITID),
    ("IDAny", TypeVal TIDAny),
    ("Type", TypeVal TType),
    ("?", TypeVal TAny)
  ]

levelenv :: [(Expression, Expression)] -> Env
levelenv vals = foldl (\li x -> successOr ((: li) <$> x) li) [] (map get_id vals)
  where
    get_id (Quote (ID id), v) = Success (id, v)
    get_id (x, _) = Error $ show x ++ " is not an id"
