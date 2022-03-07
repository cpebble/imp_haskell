module ImpAst where

-- Terminals
--- n
type Constant = Integer

-- Keyword
type Location = String

-- Bool
data Boolean = ITrue | IFalse
    deriving (Show, Eq)

-- NonTerminals 
-- Arithmetic
data AExp
  = Const Constant
  | Loc Location
  | Plus AExp AExp
  | Minus AExp AExp
  | Times AExp AExp
  deriving (Show)

-- Boolean expressions
data BExp
  = Bool Boolean
  | Eq AExp AExp
  | Leq AExp AExp
  | Not BExp
  | And BExp BExp
  deriving (Show)

-- Commands
data CExp
  = Skip
  | Assign Location AExp
  | Sequence CExp CExp
  | If BExp CExp CExp
  | While BExp CExp
  deriving (Show)

type Program = CExp
