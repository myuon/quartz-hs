module Language.Quartz.AST where

import Data.Primitive.Array
import Data.Dynamic
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
  | Add
  | Sub
  | Mult
  | Div
  | Leq
  | Lt
  | Geq
  | Gt
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
  | Any (Dynamic' posn)
  | Stmt (Expr posn)
  deriving (Eq, Show)

data Dynamic' posn = Dynamic' (Maybe posn) Dynamic
  deriving (Show)

instance Eq (Dynamic' posn) where
  _ == _ = False

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

data ArgType = ArgType Bool [(String, Type)]
  deriving (Eq, Show)

listArgTypes :: ArgType -> [Type]
listArgTypes (ArgType self xs) =
  (if self then (SelfType :) else id) $ map snd xs

listArgNames :: ArgType -> [String]
listArgNames (ArgType self xs) = (if self then ("self" :) else id) $ map fst xs

data FuncType = FuncType [String] ArgType Type
  deriving (Eq, Show)

data Closure posn = Closure FuncType (Expr posn)
  deriving (Eq, Show)

data Decl posn
  = Enum String [String] [EnumField]
  | Record String [String] [RecordField]
  | OpenD Id
  | Func String (Closure posn)
  | ExternalFunc String FuncType
  | Interface String [String] [(String, FuncType)]
  | Derive String [String] (Maybe Type) [Decl posn]
  deriving (Eq, Show)

data EnumField = EnumField String [Type]
  deriving (Eq, Show)

data RecordField = RecordField String Type
  deriving (Eq, Show)

schemeOfArgs :: FuncType -> Scheme
schemeOfArgs at@(FuncType vars _ _) = Scheme vars (typeOfArgs at)

typeOfArgs :: FuncType -> Type
typeOfArgs (FuncType _ (ArgType self args) ret) =
  FnType ((if self then (SelfType :) else id) $ map snd args) ret
