{-# LANGUAGE QuasiQuotes #-}
module Language.Quartz.Tools.SpecFmt where

import qualified Data.Vector as V
import Language.Quartz.Tools.FmtLexer
import Language.Quartz.Tools.Fmt
import Test.Tasty.Hspec hiding (Failure, Success)
import Text.RawString.QQ

scan = map (\(Lexeme _ t) -> t) . alexScanTokens

spec_lexer :: Spec
spec_lexer = do
  describe "lexer" $ do
    it "" $ do
      scan [r|let a = {
  print("foo");
  10
}|] `shouldBe` [
        Token "let",
        Token "a",
        TSymbol "=",
        TSymbol "{",
        Token "print",
        TSymbol "(",
        TStrLit "foo",
        TSymbol ")",
        TSymbol ";",
        Token "10",
        TSymbol "}"
        ]

spec_fmt :: Spec
spec_fmt = do
  describe "fmt" $ do
    it "" $ do
      parse (alexScanTokens "(10)")
        `shouldBe` [
          Scope "(" "," ")" (V.fromList [
            Tokens (V.fromList [
              Lexeme (AlexPn 1 1 2) (Token "10")
            ])
          ])
        ]

    it "" $ do
      parse (alexScanTokens "(10,20,30,40)")
        `shouldBe` [
          Scope "(" "," ")" (V.fromList [
            Tokens (V.fromList [
              Lexeme (AlexPn 1 1 2) (Token "10")
            ]),
            Tokens (V.fromList [
              Lexeme (AlexPn 4 1 5) (Token "20")
            ]),
            Tokens (V.fromList [
              Lexeme (AlexPn 7 1 8) (Token "30")
            ]),
            Tokens (V.fromList [
              Lexeme (AlexPn 10 1 11) (Token "40")
            ])
          ])
        ]

    it "" $ do
      parse (alexScanTokens "(a: int, b: string, c: Hello<A,B,C>)")
        `shouldBe` [
          Scope "(" "," ")" (V.fromList [
            Tokens (V.fromList [
              Lexeme (AlexPn 1 1 2) (Token "a"),
              Lexeme (AlexPn 2 1 3) (TSymbol ":"),
              Lexeme (AlexPn 4 1 5) (Token "int")
            ]),
            Tokens (V.fromList [
              Lexeme (AlexPn 9 1 10) (Token "b"),
              Lexeme (AlexPn 10 1 11) (TSymbol ":"),
              Lexeme (AlexPn 12 1 13) (Token "string")
            ]),
            Tokens (V.fromList [
              Lexeme (AlexPn 20 1 21) (Token "c"),
              Lexeme (AlexPn 21 1 22) (TSymbol ":"),
              Lexeme (AlexPn 23 1 24) (Token "Hello")
            ]),
            Scope "<" "," ">" (V.fromList [
              Tokens (V.fromList [
                Lexeme (AlexPn 29 1 30) (Token "A")
              ]),
              Tokens (V.fromList [
                Lexeme (AlexPn 31 1 32) (Token "B")
              ]),
              Tokens (V.fromList [
                Lexeme (AlexPn 33 1 34) (Token "C")
              ])
            ])
          ])
        ]
