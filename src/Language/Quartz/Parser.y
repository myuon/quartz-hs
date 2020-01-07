{
module Language.Quartz.Parser where

import Language.Quartz.Lexer
import Language.Quartz.AST
}

%name parser decl
%name parserDecls decls
%name parserExpr expr
%tokentype { Lexeme }
%monad { Either String } { (>>=) } { return }

%token
    '<' { Lexeme _ TLAngle }
    '>' { Lexeme _ TRAngle }
    '(' { Lexeme _ TLParen }
    ')' { Lexeme _ TRParen }
    '{' { Lexeme _ TLBrace }
    '}' { Lexeme _ TRBrace }
    '[' { Lexeme _ TLBracket }
    ']' { Lexeme _ TRBracket }
    ',' { Lexeme _ TComma }
    ':' { Lexeme _ TColon }
    ';' { Lexeme _ TSemiColon }
    '.' { Lexeme _ TDot }
    '->' { Lexeme _ TArrow }
    '=>' { Lexeme _ TDArrow }
    '=' { Lexeme _ TEq }
    '_' { Lexeme _ TUnderscore }
    '==' { Lexeme _ TEq2 }
    '::' { Lexeme _ TColon2 }
    '+' { Lexeme _ TPlus }
    '-' { Lexeme _ TMinus }
    '*' { Lexeme _ TStar }
    '/' { Lexeme _ TSlash }
    '<=' { Lexeme _ TLeq }
    '>=' { Lexeme _ TGeq }

    FUNC { Lexeme _ TFunc }
    ENUM { Lexeme _ TEnum }
    RECORD { Lexeme _ TRecord }
    OPEN { Lexeme _ TOpen }
    LET { Lexeme _ TLet }
    SELF { Lexeme _ TSelf }
    MATCH { Lexeme _ TMatch }
    EXTERNAL { Lexeme _ TExternal }
    FOR { Lexeme _ TFor }
    IN { Lexeme _ TIn }
    IF { Lexeme _ TIf }
    ELSE { Lexeme _ TElse }
    TRUE { Lexeme _ TTrue }
    FALSE { Lexeme _ TFalse }
    INTERFACE { Lexeme _ TInterface }
    DERIVE { Lexeme _ TDerive }
    REF { Lexeme _ TRef }

    INT { Lexeme _ (TInt $$) }
    STRLIT { Lexeme _ (TStrLit $$) }
    VAR { Lexeme posn (TVar $$) }

%left '+' '-'
%left '*' '/'
%%

decl :: { Decl AlexPosn }
decl
    : EXTERNAL FUNC VAR may_generics '(' arg_types ')' may_return_type ';'  { ExternalFunc $3 (FuncType $4 (ArgType False False $6) (maybe unitType id $8)) }
    | FUNC VAR may_generics '(' self_arg_types ')' may_return_type '{' stmts '}'  { Func $2 (Closure (FuncType $3 $5 (maybe unitType id $7)) (Procedure $9)) }
    | ENUM VAR may_generics '{' enum_fields '}'  { Enum $2 $3 $5 }
    | RECORD VAR may_generics '{' record_fields '}'  { Record $2 $3 $5 }
    | OPEN path ';'  { OpenD (Id $2) }
    | INTERFACE VAR may_generics '{' func_type_decls '}'  { Interface $2 $3 $5 }
    | DERIVE VAR may_generics may_for_trait '{' decls '}'  { Derive $2 $3 $4 $6 }

may_for_trait :: { Maybe Type }
may_for_trait
    : FOR type  { Just $2 }
    | {- empty -}  { Nothing }

decls :: { [Decl AlexPosn] }
decls
    : decl decls  { $1 : $2 }
    | {- empty -}  { [] }

func_type_decls :: { [(String, FuncType)] }
func_type_decls
    : {- empty -}  { [] }
    | FUNC VAR may_generics '(' self_arg_types ')' may_return_type ';' func_type_decls  { (,) $2 (FuncType $3 $5 (maybe unitType id $7)) : $9 }

path :: { [String] }
path
    : VAR  { [$1] }
    | VAR '::' path  { $1 : $3 }
    | '*'  { ["*"] }
    | {- empty -}  { [] }

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
    | VAR ',' may_generics_internal  { $1 : $3 }

may_return_type :: { Maybe Type }
may_return_type
    : ':' type  { Just $2 }
    | {- empty -}  { Nothing }

stmt :: { Expr AlexPosn }
stmt
    : FOR VAR IN expr_short '{' stmts '}'  { ForIn $2 $4 $6 }
    | IF '{' if_branches '}'  { If $3 }
    | LET REF VAR '=' expr ';'  { Stmt (LetRef $3 $5) }
    | LET VAR '=' expr ';'  { Stmt (Let (Id [$2]) $4) }
    | expr_short '=' expr ';'  { Stmt (Assign $1 $3) }
    | expr ';'  { Stmt $1 }

stmts :: { [Expr AlexPosn] }
stmts
    : {- empty -}  { [] }
    | expr  { [$1] }
    | stmt stmts  { $1 : $2 }

expr_short :: { Expr AlexPosn }
expr_short
    : literal  { Lit $1 }
    | var  { Var Nothing (Id $1) }
    | SELF  { Self SelfType }
    | expr_short args  { FnCall $1 $2 }
    | expr_short '.' VAR  { Member $1 $3 }
    | expr_short '[' expr ']'  { IndexArray $1 $3 }
    | '(' expr ')'  { $2 }
    | '{' stmts '}'  { Procedure $2 }
    | '[' array_lit ']'  { ArrayLit $2 }
    | '*' expr  { Deref $2 }

expr :: { Expr AlexPosn }
expr
    : MATCH expr_short '{' match_branches '}'  { Match $2 $4 }
    | IF '{' if_branches '}'  { If $3 }

    -- こうしないとちゃんとパースできないので(先読みの問題？)
    | '(' arg_types ')' may_return_type '->' expr  { ClosureE (Closure (FuncType [] (ArgType False False $2) (maybe unitType id $4)) $6) }
    | '<' may_generics_internal '>' '(' arg_types ')' may_return_type '->' expr  { ClosureE (Closure (FuncType $2 (ArgType False False $5) (maybe unitType id $7)) $9) }

    -- 演算子優先順位のためにはここはまとめて書いてはいけない？
    | expr '+' expr { Op Add $1 $3 }
    | expr '-' expr { Op Sub $1 $3 }
    | expr '*' expr { Op Mult $1 $3 }
    | expr '/' expr { Op Div $1 $3 }
    | expr '<=' expr { Op Leq $1 $3 }
    | expr '<' expr { Op Lt $1 $3 }
    | expr '>=' expr { Op Geq $1 $3 }
    | expr '>' expr { Op Gt $1 $3 }
    | expr '==' expr { Op Eq $1 $3 }

    | VAR '{' record_expr '}'  { RecordOf $1 $3 }

    | expr_short  { $1 }

record_expr :: { [(String, Expr AlexPosn)] }
record_expr
    : VAR ':' expr  { [($1, $3)] }
    | VAR ':' expr ',' record_expr  { ($1,$3) : $5 }
    | {- empty -}  { [] }

match_branches :: { [(Pattern, Expr AlexPosn)] }
match_branches
    : {- empty -}  { [] }
    | pattern '=>' expr  { [($1, $3)] }
    | pattern '=>' expr ',' match_branches  { ($1, $3) : $5 }

if_branches :: { [(Expr AlexPosn, Expr AlexPosn)] }
if_branches
    : {- empty -}  { [] }
    | expr '=>' expr  { [($1,$3)] }
    | expr '=>' expr ',' if_branches  { ($1, $3) : $5 }

pattern :: { Pattern }
pattern
    : '_'  { PAny }
    | pattern '(' patterns ')'  { PApp $1 $3 }
    | var  { PVar (Id $1) }
    | literal  { PLit $1 }

patterns :: { [Pattern] }
patterns
    : pattern  { [$1] }
    | pattern ',' patterns  { $1 : $3 }

args :: { [Expr AlexPosn] }
args
    : '(' ')'  { [] }
    | '(' exprs_comma ')'  { $2 }

exprs_comma :: { [Expr AlexPosn] }
exprs_comma
    : expr ',' exprs_comma { $1 : $3 }
    | expr { [$1] }

arg_types :: { [(String, Type)] }
arg_types
    : VAR ':' type ',' arg_types  { ($1, $3) : $5 }
    | VAR ':' type  { [($1, $3)] }
    | {- empty -}  { [] }

self_arg_types :: { ArgType }
self_arg_types
    : SELF ',' arg_types  { ArgType False True $3 }
    | SELF  { ArgType False True [] }
    | REF SELF ',' arg_types  { ArgType True True $4 }
    | REF SELF  { ArgType True True [] }
    | arg_types  { ArgType False False $1 }

literal :: { Literal }
literal
    : INT  { IntLit $1 }
    | STRLIT  { StringLit $1 }
    | TRUE  { BoolLit True }
    | FALSE  { BoolLit False }

array_lit :: { [Expr AlexPosn] }
array_lit
    : {- empty -}  { [] }
    | expr  { [$1] }
    | expr ',' array_lit  { $1 : $3 }

type :: { Type }
type
    : '(' ')'  { ConType (Id ["unit"]) }
    | '_'  { NoType }
    | SELF { SelfType }
    | VAR  { ConType (Id [$1]) }
    | REF '<' type '>'  { RefType $3 }
    | type '<' types_comma '>'  { AppType $1 $3 }

types_comma :: { [Type] }
types_comma
    : {- empty -}  { [] }
    | type  { [$1] }
    | type ',' types_comma  { $1 : $3 }

var :: { [String] }
var
    : VAR  { [$1] }
    | VAR '::' var  { $1 : $3 }

{
happyError tokens = Left $ "Parse error\n" ++ show tokens

unitType = ConType (Id ["unit"])
}
