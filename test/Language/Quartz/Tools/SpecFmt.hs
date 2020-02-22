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
          Scope (V.fromList [
            Tokens (V.fromList [
              Lexeme (AlexPn 1 1 2) (Token "10")
            ])
          ])
        ]

    it "" $ do
      parse (alexScanTokens "(10,20,30,40)")
        `shouldBe` [
          Scope (V.fromList [
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
