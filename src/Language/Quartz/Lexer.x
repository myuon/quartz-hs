{
module Language.Quartz.Lexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
  $white+ ;
  func { \_ -> TFunc }
  enum { \_ -> TEnum }
  record { \_ -> TRecord }
  instance { \_ -> TInstance }
  open { \_ -> TOpen }
  let { \_ -> TLet }
  self { \_ -> TSelf }
  match { \_ -> TMatch }
  \< { \_ -> TLAngle }
  \> { \_ -> TRAngle }
  \( { \_ -> TLParen }
  \) { \_ -> TRParen }
  \{ { \_ -> TLBrace }
  \} { \_ -> TRBrace }
  \, { \_ -> TComma }
  \: { \_ -> TColon }
  \; { \_ -> TSemiColon }
  \. { \_ -> TDot }
  \-> { \_ -> TArrow }
  \* { \_ -> TStar }
  \= { \_ -> TEq }
  \_ { \_ -> TUnderscore }
  $digit+ { TInt . read }
  [$alpha \_] [$alpha $digit \_]* { TVar }

{
data Token
  = TFunc
  | TEnum
  | TRecord
  | TInstance
  | TOpen
  | TLet
  | TSelf
  | TMatch
  | TLAngle
  | TRAngle
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TComma
  | TColon
  | TSemiColon
  | TDot
  | TArrow
  | TStar
  | TEq
  | TUnderscore
  | TInt Int
  | TVar String
  deriving (Eq, Show)
}
