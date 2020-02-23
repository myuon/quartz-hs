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
            Sequence (V.fromList [
              AToken $ Lexeme (AlexPn 1 1 2) (Token "10")
            ])
          ])
        ]

    it "" $ do
      parse (alexScanTokens "(10,20,30,40)")
        `shouldBe` [
          Scope "(" "," ")" (V.fromList [
            Sequence (V.fromList [
              AToken $ Lexeme (AlexPn 1 1 2) (Token "10")
            ]),
            Sequence (V.fromList [
              AToken $ Lexeme (AlexPn 4 1 5) (Token "20")
            ]),
            Sequence (V.fromList [
              AToken $ Lexeme (AlexPn 7 1 8) (Token "30")
            ]),
            Sequence (V.fromList [
              AToken $ Lexeme (AlexPn 10 1 11) (Token "40")
            ])
          ])
        ]

    it "" $ do
      parse (alexScanTokens "(a: int, b: string, c: Hello<A,B,C>)")
        `shouldBe` [
          Scope "(" "," ")" (V.fromList [
            Sequence (V.fromList [
              AToken $ Lexeme (AlexPn 1 1 2) (Token "a"),
              AToken $ Lexeme (AlexPn 2 1 3) (TSymbol ":"),
              AToken $ Lexeme (AlexPn 4 1 5) (Token "int")
            ]),
            Sequence (V.fromList [
              AToken $ Lexeme (AlexPn 9 1 10) (Token "b"),
              AToken $ Lexeme (AlexPn 10 1 11) (TSymbol ":"),
              AToken $ Lexeme (AlexPn 12 1 13) (Token "string")
            ]),
            Sequence (V.fromList [
              AToken $ Lexeme (AlexPn 20 1 21) (Token "c"),
              AToken $ Lexeme (AlexPn 21 1 22) (TSymbol ":"),
              AToken $ Lexeme (AlexPn 23 1 24) (Token "Hello"),
              Scope "<" "," ">" (V.fromList [
                Sequence (V.fromList [
                  AToken $ Lexeme (AlexPn 29 1 30) (Token "A")
                ]),
                Sequence (V.fromList [
                  AToken $ Lexeme (AlexPn 31 1 32) (Token "B")
                ]),
                Sequence (V.fromList [
                  AToken $ Lexeme (AlexPn 33 1 34) (Token "C")
                ])
              ])
            ])
          ])
        ]

    it "" $ do
      parse (alexScanTokens "{ println(\"foo\"); let a = 20; a * 2 }")
        `shouldBe` [
          Scope "{" ";" "}" (V.fromList [
            Sequence (V.fromList [
              AToken $ Lexeme (AlexPn 2 1 3) (Token "println"),
              Scope "(" "," ")" (V.fromList [
                Sequence (V.fromList [
                  AToken $ Lexeme (AlexPn 10 1 11) (TStrLit "foo")
                ])
              ])
            ]),
            Sequence (V.fromList [
              AToken $ Lexeme (AlexPn 18 1 19) (Token "let"),
              AToken $ Lexeme (AlexPn 22 1 23) (Token "a"),
              AToken $ Lexeme (AlexPn 24 1 25) (TSymbol "="),
              AToken $ Lexeme (AlexPn 26 1 27) (Token "20")
            ]),
            Sequence (V.fromList [
              AToken $ Lexeme (AlexPn 30 1 31) (Token "a"),
              AToken $ Lexeme (AlexPn 32 1 33) (TSymbol "*"),
              AToken $ Lexeme (AlexPn 34 1 35) (Token "2")
            ])
          ])
        ]
