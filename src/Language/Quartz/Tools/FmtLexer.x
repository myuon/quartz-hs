{
module Language.Quartz.Tools.FmtLexer where
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]
@token = [$alpha $digit \_ \:\:]+
@string = \" ($printable # \")* \"

tokens :-
  $white+ ;
  "//".*  { wrap $ TLineComment }

  \< { wrap $ TScopeSymbol }
  \> { wrap $ TScopeSymbol }
  \( { wrap $ TScopeSymbol }
  \) { wrap $ TScopeSymbol }
  \[ { wrap $ TScopeSymbol }
  \] { wrap $ TScopeSymbol }
  \, { wrap $ TScopeSymbol }
  \: { wrap $ TSymbol }
  \{ { wrap $ TBlockSymbol }
  \} { wrap $ TBlockSymbol }
  \; { wrap $ TBlockSymbol }
  \. { wrap $ TSymbol }
  \-> { wrap $ TSymbol }
  \=> { wrap $ TSymbol }
  \= { wrap $ TSymbol }
  \== { wrap $ TSymbol }
  \+ { wrap $ TSymbol }
  \- { wrap $ TSymbol }
  \* { wrap $ TSymbol }
  \/ { wrap $ TSymbol }
  \<= { wrap $ TSymbol }
  \>= { wrap $ TSymbol }
  @token { wrap Token }
  @string { wrap (\s -> TStrLit $ init $ tail s) }

{
data Token
  = Token String
  | TScopeSymbol String
  | TBlockSymbol String
  | TSymbol String
  | TStrLit String
  | TLineComment String
  deriving (Eq, Show)

data Lexeme = Lexeme AlexPosn Token
  deriving (Eq, Show)

posOfLexeme :: Lexeme -> AlexPosn
posOfLexeme (Lexeme p _) = p

wrap :: (String -> Token) -> AlexPosn -> String -> Lexeme
wrap f p s = Lexeme p (f s)
}
