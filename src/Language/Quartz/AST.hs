module Language.Quartz.AST where

import Data.Primitive.Array
import Control.Monad.Primitive (RealWorld)

data Literal
  = IntLit Int
  | DoubleLit Double
  | CharLit Char
  | StringLit String
  | BoolLit Bool
  deriving (Eq, Show)

data Id = Id [String]
  deriving (Eq, Ord, Show)

data Op
  = Eq
  deriving (Eq, Show)

data Expr posn
  = Var (Maybe posn) Id
  | Lit Literal
  | FnCall (Expr posn) [Expr posn]
  | Let Id (Expr posn)
  | ClosureE (Closure posn)
  | OpenE Id
  | Match (Expr posn) [(Pattern, Expr posn)]
  | If [(Expr posn, Expr posn)]
  | Procedure [Expr posn]
  | Unit
  | FFI Id [Expr posn]
  -- primitiveのときはMutableByteArrayにしたい
  | Array (MArray posn)
  | ArrayLit [Expr posn]
  | IndexArray (Expr posn) (Expr posn)
  | ForIn String (Expr posn) [Expr posn]
  | Op Op (Expr posn) (Expr posn)
  | Member (Expr posn) String
  | RecordOf String [(String, Expr posn)]
  | EnumOf Id [Expr posn]
  | Assign (Expr posn) (Expr posn)
  | Self Type
  | MethodOf Type String (Expr posn)
  deriving (Eq, Show)

newtype MArray posn = MArray { getMArray :: MutableArray RealWorld (Expr posn) }
  deriving Eq

instance Show (MArray posn) where
  show _ = "<<array>>"

data Type
  = ConType Id
  | VarType String
  | AppType Type [Type]
  | SelfType
  | NoType
  | FnType [Type] Type
  deriving (Eq, Show, Ord)

data Scheme = Scheme [String] Type
  deriving (Eq, Show)

data Pattern
  = PVar Id
  | PLit Literal
  | PApp Pattern [Pattern]
  | PAny
  deriving (Eq, Show)

data ArgTypes = ArgTypes [String] [(String, Type)] Type
  deriving (Eq, Show)

data Closure posn = Closure ArgTypes (Expr posn)
  deriving (Eq, Show)

data FuncType = FuncType String ArgTypes
  deriving (Eq, Show)

nameOfFuncType :: FuncType -> String
nameOfFuncType (FuncType name _) = name

data Decl posn
  = Enum String [String] [EnumField]
  | Record String [String] [RecordField]
  | OpenD Id
  | Func String (Closure posn)
  | ExternalFunc String ArgTypes
  | Interface String [String] [FuncType]
  | Derive String [String] (Maybe Type) [Decl posn]
  deriving (Eq, Show)

data EnumField = EnumField String [Type]
  deriving (Eq, Show)

data RecordField = RecordField String Type
  deriving (Eq, Show)

schemeOfArgs :: ArgTypes -> Scheme
schemeOfArgs at@(ArgTypes vars _ _) = Scheme vars (typeOfArgs at)

typeOfArgs :: ArgTypes -> Type
typeOfArgs (ArgTypes _ args ret) = FnType (map snd args) ret
