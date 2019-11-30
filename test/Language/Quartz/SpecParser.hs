{-# LANGUAGE QuasiQuotes #-}
module Language.Quartz.SpecParser where

import Language.Quartz.Lexer (alexScanTokens)
import Language.Quartz.AST
import Language.Quartz.Parser
import Test.Tasty.Hspec hiding (Failure, Success)
import Text.RawString.QQ

parseE = either error id . parserExpr . alexScanTokens
parseD = either error id . parser . alexScanTokens
parseDs = either error id . parserDecls . alexScanTokens

spec_parser :: Spec
spec_parser = do
  describe "parser" $ do
    it "should parse" $ do
      parseE "xxx" `shouldBe` Var (Id ["xxx"])

      parseE "10" `shouldBe` Lit (IntLit 10)

      parseE [r| "aaa" |] `shouldBe` Lit (StringLit "\"aaa\"")
      parseE [r| "あああ" |] `shouldBe` Lit (StringLit "\"あああ\"")

      parseE "foo(x,y,z)" `shouldBe` FnCall
        (Var (Id ["foo"]))
        [Var (Id ["x"]), Var (Id ["y"]), Var (Id ["z"])]

      parseE "x.foo(y,z)" `shouldBe` FnCall (Var (Id ["x", "foo"]))
                                            [Var (Id ["y"]), Var (Id ["z"])]

      parseE "a.b.c" `shouldBe` Var (Id ["a", "b", "c"])

      parseE "[1,2,3,4]" `shouldBe` ArrayLit [Lit (IntLit 1),Lit (IntLit 2),Lit (IntLit 3),Lit (IntLit 4)]

      parseE "h(f)[g(1)]" `shouldBe` IndexArray (FnCall (Var (Id ["h"])) [Var (Id ["f"])]) (FnCall (Var (Id ["g"])) [Lit (IntLit 1)])

      parseE "(a: string): string -> a" `shouldBe` ClosureE
        ( Closure
          (Scheme [] $ ConType (Id ["string"]) `ArrowType` ConType (Id ["string"]))
          ["a"]
          (Var (Id ["a"]))
        )

      parseE "<A>(a: A): A -> a" `shouldBe` ClosureE
        ( Closure
          (Scheme ["A"] (VarType "A" `ArrowType` VarType "A"))
          ["a"]
          (Var (Id ["a"]))
        )


      parseE "(a: int, b: int, c: int) -> { let z = sum(a,b,c); z }"
        `shouldBe` ClosureE
                     ( Closure
                       ( Scheme [] $ ConType (Id ["int"])
                       `ArrowType` (           ConType (Id ["int"])
                                   `ArrowType` (           ConType (Id ["int"])
                                               `ArrowType` ConType
                                                             (Id ["unit"])
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

      parseE [r|
        {
          let f = (): int -> 10;
          f
        }
      |] `shouldBe` Procedure [Let (Id ["f"]) (ClosureE (Closure (Scheme [] $ ConType (Id ["unit"]) `ArrowType` ConType (Id ["int"])) ["()"] (Lit (IntLit 10)))), Var (Id ["f"])]

      parseD "func id(x: A): A { let y = x; y }" `shouldBe` Func
        "id"
        ( Closure
          (Scheme [] $ ArrowType (ConType (Id ["A"])) (ConType (Id ["A"])))
          ["x"]
          (Procedure [Let (Id ["y"]) (Var (Id ["x"])), Var (Id ["y"])])
        )

      parseD "enum Nat { Zero, Succ(Nat) }" `shouldBe` Enum
        "Nat"
        [EnumField "Zero" [], EnumField "Succ" [ConType (Id ["Nat"])]]

      parseD "record User { user_id: string, age: int, }" `shouldBe` Record
        "User"
        [ RecordField "user_id" (ConType (Id ["string"]))
        , RecordField "age"     (ConType (Id ["int"]))
        ]

      parseD "open List.Foo.Bar.*;" `shouldBe` OpenD "List.Foo.Bar.*"

      parseD
          "instance Nat { func is_zero(self): bool { match self { Zero -> true, Succ(_) -> false } } }"
        `shouldBe` Instance
                     "Nat"
                     [ Method
                         "is_zero"
                         ( Closure
                           (Scheme [] $ ArrowType SelfType (ConType (Id ["bool"])))
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

      parseD [r|
        func main() {
          println("Hello, World!");
        }
      |] `shouldBe` Func "main" (Closure
        (Scheme [] $ ConType (Id ["unit"]) `ArrowType` ConType (Id ["unit"]))
        ["()"]
        (Procedure [FnCall (Var (Id ["println"])) [Lit (StringLit "\"Hello, World!\"")], Unit])
        )

      parseD [r|
        external func println(x: string);
      |] `shouldBe` ExternalFunc "println" (Scheme [] $ ArrowType (ConType (Id ["string"])) (ConType (Id ["unit"])))

      parseD [r|
        func f() {
          for i in foo {
            put(i);
          }
        }
      |] `shouldBe` Func "f" (Closure (Scheme [] (ArrowType (ConType (Id ["unit"])) (ConType (Id ["unit"])))) ["()"] (
                      Procedure [
                        ForIn "i" (Var (Id ["foo"])) [
                          FnCall (Var (Id ["put"])) [Var (Id ["i"])],
                          Unit
                        ],
                        Unit
                      ]
                    ))
