module Language.Quartz.SpecParser where

import Language.Quartz.Lexer (alexScanTokens)
import Language.Quartz.AST
import Language.Quartz.Parser
import Test.Tasty.Hspec hiding (Failure, Success)

parseE = (\(Right r) -> r) . parserExpr . alexScanTokens
parseD = either error id . parser . alexScanTokens

spec_parser :: Spec
spec_parser = do
  describe "parser" $ do
    it "should parse" $ do
      parseE "xxx" `shouldBe` Var "xxx"

      parseE "10" `shouldBe` Lit (IntLit 10)

      parseE "foo(x,y,z)" `shouldBe` App (Var "foo") [Var "x", Var "y", Var "z"]

      parseE "x.foo(y,z)" `shouldBe` App (Var "foo") [Var "x", Var "y", Var "z"]

      parseD "func id(x: A): A { let y = x; y }" `shouldBe` Func
        "id"
        ( Closure (ArrowType (VarType "A") (VarType "A"))
                  ["x"]
                  [Let "y" (Var "x"), Var "y"]
        )

      parseD "enum Nat { Zero, Succ(Nat) }" `shouldBe` Enum
        "Nat"
        [EnumField "Zero" [], EnumField "Succ" [VarType "Nat"]]

      parseD "record User { user_id: string, age: int, }" `shouldBe` Record
        "User"
        [ RecordField "user_id" (VarType "string")
        , RecordField "age"     (VarType "int")
        ]
