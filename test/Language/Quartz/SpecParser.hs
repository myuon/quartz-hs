module Language.Quartz.SpecParser where

import Language.Quartz.Lexer (alexScanTokens)
import Language.Quartz.AST
import Language.Quartz.Parser
import Test.Tasty.Hspec hiding (Failure, Success)

parseE = (\(Right r) -> r) . parserExpr . alexScanTokens

spec_parser :: Spec
spec_parser = do
  describe "parser" $ do
    it "should parse" $ do
      parseE "xxx" `shouldBe` Var "xxx"

      parseE "10" `shouldBe` Lit (IntLit 10)

      parseE "foo(x,y,z)" `shouldBe` App (Var "foo") [Var "x", Var "y", Var "z"]

      parseE "x.foo(y,z)" `shouldBe` App (Var "foo") [Var "x", Var "y", Var "z"]
