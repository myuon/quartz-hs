{
module Language.Quartz.Lexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
@string = \" ($printable # \")* \"

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
  external { \_ -> TExternal }
  for { \_ -> TFor }
  in { \_ -> TIn }
  if { \_ -> TIf }
  else { \_ -> TElse }

  -- 避けられるなら予約語から外したい
  true { \_ -> TTrue }
  false { \_ -> TFalse }

  \< { \_ -> TLAngle }
  \> { \_ -> TRAngle }
  \( { \_ -> TLParen }
  \) { \_ -> TRParen }
  \{ { \_ -> TLBrace }
  \} { \_ -> TRBrace }
  \[ { \_ -> TLBracket }
  \] { \_ -> TRBracket }
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
  @string { TStrLit }

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
  | TExternal
  | TFor
  | TIn
  | TIf
  | TElse
  | TTrue
  | TFalse
  | TLAngle
  | TRAngle
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TLBracket
  | TRBracket
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
  | TStrLit String
  deriving (Eq, Show)
}
