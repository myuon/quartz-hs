{
module Language.Quartz.Parser where

import Language.Quartz.Lexer
import Language.Quartz.AST
}

%name parser decl
%name parserExpr expr
%tokentype { Token }
%monad { Either String } { (>>=) } { return }

%token
    '<' { TLAngle }
    '>' { TRAngle }
    '(' { TLParen }
    ')' { TRParen }
    '{' { TLBrace }
    '}' { TRBrace }
    ',' { TComma }
    ':' { TColon }
    ';' { TSemiColon }
    '.' { TDot }
    '->' { TArrow }
    '*' { TStar }
    '=' { TEq }

    FUNC { TFunc }
    ENUM { TEnum }
    RECORD { TRecord }
    INSTANCE { TInstance }
    OPEN { TOpen }
    LET { TLet }
    SELF { TSelf }
    CASE { TCase }

    INT { TInt $$ }
    VAR { TVar $$ }

%%

decl :: { Decl }
decl
    : FUNC VAR '(' arg_types ')' may_return_type '{' stmts '}'  { Func $2 (createClosure $4 $6 $8) }
    | ENUM VAR '{' enum_fields '}'  { Enum $2 $4 }
    | RECORD VAR '{' record_fields '}'  { Record $2 $4 }

enum_fields :: { [EnumField] }
enum_fields
    : enum_field ',' enum_fields  { $1 : $3 }
    | enum_field  { [$1] }
    | {- empty -}  { [] }

enum_field :: { EnumField }
enum_field
    : VAR  { EnumField $1 [] }
    | VAR '(' type_args ')'  { EnumField $1 $3 }

type_args :: { [Type] }
type_args
    : type ',' type_args  { $1 : $3 }
    | type  { [$1] }
    | {- empty -}  { [] }

record_fields :: { [RecordField] }
record_fields
    : record_field ',' record_fields  { $1 : $3 }
    | record_field  { [$1] }
    | {- empty -}  { [] }

record_field :: { RecordField }
record_field
    : VAR ':' type  { RecordField $1 $3 }

may_return_type :: { Maybe Type }
may_return_type
    : ':' type  { Just $2 }
    | {- empty -}  { Nothing }

stmts :: { [Expr] }
stmts
    : expr ';' stmts  { $1 : $3 }
    | LET VAR '=' expr ';' stmts { Let $2 $4 : $6 }
    | expr  { [$1] }
    | {- empty -}  { [] }

expr :: { Expr }
expr
    : literal  { Lit $1 }
    | expr args  { App $1 $2 }
    | expr '.' VAR args  { App (Var $3) ($1 : $4) }
    | '(' expr ')'  { $2 }
    | VAR  { Var $1 }

args :: { [Expr] }
args
    : '(' ')'  { [] }
    | '(' exprs_comma ')'  { $2 }

exprs_comma :: { [Expr] }
exprs_comma
    : expr ',' exprs_comma { $1 : $3 }
    | expr { [$1] }

arg_types :: { [(String, Type)] }
arg_types
    : VAR ':' type ',' arg_types  { ($1, $3) : $5 }
    | VAR ':' type  { [($1, $3)] }
    | {- empty -}  { [] }

literal :: { Literal }
literal
    : INT { IntLit $1 }

type :: { Type }
type
    : '(' ')'  { UnitType }
    | VAR  { VarType $1 }

{
happyError tokens = Left $ "Parse error\n" ++ show tokens

createClosure :: [(String, Type)] -> Maybe Type -> [Expr] -> Closure
createClosure args ret es =
  let retType = maybe UnitType id ret in
  Closure (foldr ArrowType retType $ map snd args) (map fst args) es
}
