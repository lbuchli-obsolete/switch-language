module RuntimeAST where

data Expression
  = Dict [(Expression, Expression)]
  | Appl [Expression]
  | Quote Expression
  | Unquote Expression
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
  | ITQuote
  | ITFn0
  | ITFn1 Type
  | ITCh
  | ITNbr
  | ITID
  | ITType
  | ITAny
  deriving (Eq, Show)

data Type
  = TDict [(Expression, Expression)]
  | TDictLen Int
  | TDictAny
  | TAppl [Expression]
  | TApplLen Int
  | TApplAny
  | TQuote Expression
  | TFn Expression Expression
  | TCh
  | TNbr
  | TID
  | TType
  | TAny
  deriving (Eq, Show)
