{-# LANGUAGE QuasiQuotes #-}
module Language.Quartz.Tools.SpecFmtLexer where

import Language.Quartz.Tools.FmtLexer
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
        TBlockSymbol "{",
        Token "print",
        TScopeSymbol "(",
        TStrLit "foo",
        TScopeSymbol ")",
        TBlockSymbol ";",
        Token "10",
        TBlockSymbol "}"
        ]
