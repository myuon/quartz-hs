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

  \< { wrap $ TSymbol }
  \> { wrap $ TSymbol }
  \( { wrap $ TSymbol }
  \) { wrap $ TSymbol }
  \[ { wrap $ TSymbol }
  \] { wrap $ TSymbol }
  \, { wrap $ TSymbol }
  \: { wrap $ TSymbol }
  \{ { wrap $ TSymbol }
  \} { wrap $ TSymbol }
  \; { wrap $ TSymbol }
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
  | TSymbol String
  | TStrLit String
  | TLineComment String
  deriving (Eq, Show)

data Lexeme = Lexeme AlexPosn Token
  deriving (Eq, Show)

posOfLexeme :: Lexeme -> AlexPosn
posOfLexeme (Lexeme p _) = p

tokenOfLexeme :: Lexeme -> Token
tokenOfLexeme (Lexeme _ t) = t

wrap :: (String -> Token) -> AlexPosn -> String -> Lexeme
wrap f p s = Lexeme p (f s)
}
