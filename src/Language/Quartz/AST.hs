module Language.Quartz.AST where

data Literal
  = IntLit Int
  | DoubleLit Double
  | CharLit Char
  | StringLit String
  deriving (Eq, Show)

data Id = Id [String]
  deriving (Eq, Ord, Show)

data Expr
  = Var Id
  | Lit Literal
  | FnCall Expr [Expr]
  | Let Id Expr
  | ClosureE Closure
  | OpenE String
  | Match Expr [(Pattern, Expr)]
  | Procedure [Expr]
  | Unit
  | NoExpr
  deriving (Eq, Show)

data Type
  = ArrowType Type Type
  | ConType Id
  | VarType String
  | SelfType
  | NoType
  deriving (Eq, Show)

data Pattern
  = PVar String
  | PLit Literal
  | PApp Pattern [Pattern]
  | PAny
  deriving (Eq, Show)

data Closure = Closure Type [String] Expr
  deriving (Eq, Show)

data Decl
  = Enum String [EnumField]
  | Record String [RecordField]
  | Instance String [Decl]
  | OpenD String
  | Func String Closure
  | Method String Closure
  deriving (Eq, Show)

data EnumField = EnumField String [Type]
  deriving (Eq, Show)

data RecordField = RecordField String Type
  deriving (Eq, Show)
