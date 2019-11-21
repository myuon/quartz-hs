module Language.Quartz.AST where

data Literal
  = IntLit Int
  | DoubleLit Double
  | CharLit Char
  | StringLit String
  deriving (Eq, Show)

data Expr
  = Var String
  | Lit Literal
  | App Expr [Expr]
  | Let String Expr
  | ClosureE Closure
  | OpenE String
  | Case Expr Type [(Pattern, Expr)]
  deriving (Eq, Show)

data Type
  = ArrowType Type Type
  | UnitType
  deriving (Eq, Show)

data Pattern = Pattern
  deriving (Eq, Show)

data Closure = Closure Type [String] [Expr]
  deriving (Eq, Show)

data Decl
  = Enum String [EnumField]
  | Record String [RecordField]
  | Instance [Decl]
  | OpenD String
  | Func String Closure
  deriving (Eq, Show)

data EnumField = EnumField String [Type]
  deriving (Eq, Show)

data RecordField = RecordField String Type
  deriving (Eq, Show)
