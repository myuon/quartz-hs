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
      parseE "xxx" `shouldBe` Var (Id ["xxx"])

      parseE "10" `shouldBe` Lit (IntLit 10)

      parseE "foo(x,y,z)" `shouldBe` FnCall
        (Var (Id ["foo"]))
        [Var (Id ["x"]), Var (Id ["y"]), Var (Id ["z"])]

      parseE "x.foo(y,z)" `shouldBe` FnCall (Var (Id ["x", "foo"]))
                                            [Var (Id ["y"]), Var (Id ["z"])]

      parseE "a.b.c" `shouldBe` Var (Id ["a", "b", "c"])

      parseE "(a: string): string -> a" `shouldBe` ClosureE
        ( Closure (VarType "string" `ArrowType` VarType "string")
                  ["a"]
                  (Var (Id ["a"]))
        )

      parseE "(a: int, b: int, c: int) -> { let z = sum(a,b,c); z }"
        `shouldBe` ClosureE
                     ( Closure
                       (           VarType "int"
                       `ArrowType` (           VarType "int"
                                   `ArrowType` (           VarType "int"
                                               `ArrowType` UnitType
                                               )
                                   )
                       )
                       ["a", "b", "c"]
                       ( Procedure
                         [ Let
                           (Id ["z"])
                           ( FnCall
                             (Var (Id ["sum"]))
                             [Var (Id ["a"]), Var (Id ["b"]), Var (Id ["c"])]
                           )
                         , Var (Id ["z"])
                         ]
                       )
                     )

      parseD "func id(x: A): A { let y = x; y }" `shouldBe` Func
        "id"
        ( Closure
          (ArrowType (VarType "A") (VarType "A"))
          ["x"]
          (Procedure [Let (Id ["y"]) (Var (Id ["x"])), Var (Id ["y"])])
        )

      parseD "enum Nat { Zero, Succ(Nat) }" `shouldBe` Enum
        "Nat"
        [EnumField "Zero" [], EnumField "Succ" [VarType "Nat"]]

      parseD "record User { user_id: string, age: int, }" `shouldBe` Record
        "User"
        [ RecordField "user_id" (VarType "string")
        , RecordField "age"     (VarType "int")
        ]

      parseD "open List.Foo.Bar.*;" `shouldBe` OpenD "List.Foo.Bar.*"

      parseD
          "instance Nat { func is_zero(self): bool { match self { Zero -> true, Succ(_) -> false } } }"
        `shouldBe` Instance
                     "Nat"
                     [ Method
                         "is_zero"
                         ( Closure
                           (ArrowType SelfType (VarType "bool"))
                           ["self"]
                           ( Procedure
                             [ Match
                                 (Var (Id ["self"]))
                                 [ (PVar "Zero", Var (Id ["true"]))
                                 , ( PApp (PVar "Succ") [PAny]
                                   , Var (Id ["false"])
                                   )
                                 ]
                             ]
                           )
                         )
                     ]
