{
module Language.Quartz.Parser where

import Language.Quartz.Lexer
import Language.Quartz.AST
}

%name parser decl
%name parserDecls decls
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
    '[' { TLBracket }
    ']' { TRBracket }
    ',' { TComma }
    ':' { TColon }
    ';' { TSemiColon }
    '.' { TDot }
    '->' { TArrow }
    '*' { TStar }
    '=' { TEq }
    '_' { TUnderscore }

    FUNC { TFunc }
    ENUM { TEnum }
    RECORD { TRecord }
    INSTANCE { TInstance }
    OPEN { TOpen }
    LET { TLet }
    SELF { TSelf }
    MATCH { TMatch }
    EXTERNAL { TExternal }

    INT { TInt $$ }
    STRLIT { TStrLit $$ }
    VAR { TVar $$ }

%%

decl :: { Decl }
decl
    : EXTERNAL FUNC VAR may_generics '(' arg_types ')' may_return_type ';'  { ExternalFunc $3 (createFunctionType $4 $6 $8) }
    | FUNC VAR may_generics '(' arg_types ')' may_return_type '{' stmts '}'  { Func $2 (createClosure $3 $5 $7 (Procedure $9)) }
    | FUNC VAR may_generics '(' self_arg_types ')' may_return_type '{' stmts '}'  { Method $2 (createClosure $3 $5 $7 (Procedure $9)) }
    | ENUM VAR '{' enum_fields '}'  { Enum $2 $4 }
    | RECORD VAR '{' record_fields '}'  { Record $2 $4 }
    | INSTANCE VAR '{' decls '}'  { Instance $2 $4 }
    | OPEN path ';'  { OpenD $2 }

decls :: { [Decl] }
decls
    : decl decls  { $1 : $2 }
    | {- empty -}  { [] }

path :: { String }
path
    : VAR  { $1 }
    | VAR '.' path  { $1 ++ "." ++ $3 }
    | '*'  { "*" }
    | {- empty -}  { "" }

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

may_generics :: { [String] }
may_generics
    : {- empty -}  { [] }
    | '<' may_generics_internal '>'  { $2 }

may_generics_internal :: { [String] }
may_generics_internal
    : {- empty -}  { [] }
    | VAR  { [$1] }
    | VAR ',' may_generics  { $1 : $3 }

may_return_type :: { Maybe Type }
may_return_type
    : ':' type  { Just $2 }
    | {- empty -}  { Nothing }

stmts :: { [Expr] }
stmts
    : expr ';' stmts  { $1 : $3 }
    | LET VAR '=' expr ';' stmts { Let (Id [$2]) $4 : $6 }
    | expr  { [$1] }
    | {- empty -}  { [Unit] }

expr :: { Expr }
expr
    : literal  { Lit $1 }
    | MATCH expr '{' match_branches '}'  { Match $2 $4 }
    | expr args  { FnCall $1 $2 }
    | expr '[' expr ']'  { IndexArray $1 $3 }

    -- ここをmay_genericsでまとめると動かなくなる？
    | '<' may_generics_internal '>' '(' arg_types ')' may_return_type '->' expr  { ClosureE (createClosure $2 $5 $7 $9) }
    | '(' arg_types ')' may_return_type '->' expr  { ClosureE (createClosure [] $2 $4 $6) }

    | '(' expr ')'  { $2 }
    | '{' stmts '}'  { Procedure $2 }
    | var  { Var $1 }
    | SELF  { Var (Id ["self"]) }
    | '[' array_lit ']'  { ArrayLit $2 }

match_branches :: { [(Pattern, Expr)] }
match_branches
    : {- empty -}  { [] }
    | pattern '->' expr  { [($1, $3)] }
    | pattern '->' expr ',' match_branches  { ($1, $3) : $5 }

pattern :: { Pattern }
pattern
    : '_'  { PAny }
    | pattern '(' patterns ')'  { PApp $1 $3 }
    | VAR  { PVar $1 }
    | literal  { PLit $1 }

patterns :: { [Pattern] }
patterns
    : pattern  { [$1] }
    | pattern ',' patterns  { $1 : $3 }

args :: { [Expr] }
args
    : '(' ')'  { [Unit] }
    | '(' exprs_comma ')'  { $2 }

exprs_comma :: { [Expr] }
exprs_comma
    : expr ',' exprs_comma { $1 : $3 }
    | expr { [$1] }

arg_types :: { [(String, Type)] }
arg_types
    : VAR ':' type ',' arg_types  { ($1, $3) : $5 }
    | VAR ':' type  { [($1, $3)] }
    | {- empty -}  { [("()", ConType (Id ["unit"]))] }

self_arg_types :: { [(String, Type)] }
self_arg_types
    : SELF ',' arg_types  { ("self", SelfType) : $3 }
    | SELF  { [("self", SelfType)] }

literal :: { Literal }
literal
    : INT  { IntLit $1 }
    | STRLIT  { StringLit $1 }

array_lit :: { [Expr] }
array_lit
    : {- empty -}  { [] }
    | expr  { [$1] }
    | expr ',' array_lit  { $1 : $3 }

type :: { Type }
type
    : '(' ')'  { ConType (Id ["unit"]) }
    | '_'  { NoType }
    | VAR  { ConType (Id [$1]) }

var :: { Id }
var
    : var_internal  { Id $1 }

var_internal :: { [String] }
var_internal
    : VAR  { [$1] }
    | VAR '.' var_internal  { $1 : $3 }

{
happyError tokens = Left $ "Parse error\n" ++ show tokens

toVarType :: [String] -> Type -> Type
toVarType vars t = case t of
    ConType (Id [i]) | i `elem` vars -> VarType i
    _ -> t

createFunctionType :: [String] -> [(String, Type)] -> Maybe Type -> Scheme
createFunctionType vars args ret = 
  let args' = map (\(s,t) -> (s, toVarType vars t)) args in
  let retType = toVarType vars $ maybe (ConType (Id ["unit"])) id ret in
  Scheme vars $ foldr ArrowType retType $ map snd args'

createClosure :: [String] -> [(String, Type)] -> Maybe Type -> Expr -> Closure
createClosure vars args ret body = Closure (createFunctionType vars args ret) (map fst args) body
}
