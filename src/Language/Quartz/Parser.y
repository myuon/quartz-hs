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
    : FUNC VAR '(' arg_types ')' may_return_type '{' exprs '}'  { Func $2 (createClosure $4 $6 $8) }

may_return_type :: { Maybe Type }
may_return_type
    : ':' type  { Just $2 }
    | {- empty -}  { Nothing }

exprs :: { [Expr] }
exprs
    : expr ';' exprs  { $1 : $3 }
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
    | {- empty -}  { [] }

literal :: { Literal }
literal
    : INT { IntLit $1 }

type :: { Type }
type
    : '(' ')'  { UnitType }

{
happyError tokens = Left $ "Parse error\n" ++ show tokens

createClosure :: [(String, Type)] -> Maybe Type -> [Expr] -> Closure
createClosure args ret es =
  let retType = maybe UnitType id ret in
  Closure (foldr ArrowType retType $ map snd args) (map fst args) es
}
