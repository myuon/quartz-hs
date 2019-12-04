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

      parseE "x.foo(y,z)" `shouldBe` FnCall (Member (Var (Id ["x"])) "foo")
                                            [Var (Id ["y"]), Var (Id ["z"])]

      parseE "a.b.c" `shouldBe` Member (Member (Var (Id ["a"])) "b") "c"

      parseE "[1,2,3,4]" `shouldBe` ArrayLit [Lit (IntLit 1),Lit (IntLit 2),Lit (IntLit 3),Lit (IntLit 4)]

      parseE "h(f)[g(1)]" `shouldBe` IndexArray (FnCall (Var (Id ["h"])) [Var (Id ["f"])]) (FnCall (Var (Id ["g"])) [Lit (IntLit 1)])

      parseE [r|
        if {
          b1 => e1,
          b2 => e2,
          true => e3,
        }
      |] `shouldBe` If [
        (Var (Id ["b1"]), Var (Id ["e1"])),
        (Var (Id ["b2"]), Var (Id ["e2"])),
        (Lit (BoolLit True), Var (Id ["e3"]))
        ]

      parseE [r|
        if {
          0 == 1 => "true",
          true => "false",
        }
      |] `shouldBe` If [
        (Op Eq (Lit (IntLit 0)) (Lit (IntLit 1)), Lit (StringLit "\"true\"")),
        (Lit (BoolLit True), Lit (StringLit "\"false\""))
        ]

      parseE "(a: string): string -> { a }" `shouldBe` ClosureE
        ( Closure
          (ArgTypes [] [("a", ConType (Id ["string"]))] (ConType (Id ["string"])))
          (Procedure [Var (Id ["a"])])
        )

      parseE "<A>(a: A): A -> a" `shouldBe` ClosureE
        ( Closure
          (ArgTypes ["A"] [("a", VarType "A")] (VarType "A"))
          (Var (Id ["a"]))
        )

      parseE [r|
        Pos {
          x: 10,
          y: "foo"
        }
      |] `shouldBe` RecordOf "Pos" [("x", Lit (IntLit 10)), ("y", Lit (StringLit "\"foo\""))]

      parseE "(a: int, b: int, c: int) -> { let z = sum(a,b,c); z }"
        `shouldBe` ClosureE
                     ( Closure
                       (ArgTypes []
                       [
                         ("a", ConType (Id ["int"])),
                         ("b", ConType (Id ["int"])),
                         ("c", ConType (Id ["int"]))
                       ]
                       (ConType (Id ["unit"])))
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
          let f = (): int -> { 10 };
          f
        }
      |] `shouldBe` Procedure [Let (Id ["f"]) (ClosureE (Closure (ArgTypes [] [("()", ConType (Id ["unit"]))] (ConType (Id ["int"]))) (Procedure [Lit (IntLit 10)]))), Var (Id ["f"])]

      parseD "func id<A>(x: A): A { let y = x; y }" `shouldBe` Func
        "id"
        ( Closure
          (ArgTypes ["A"]
          [("x", VarType "A")]
          (VarType "A"))
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

      parseD "open List::Foo::Bar::*;" `shouldBe` OpenD (Id ["List", "Foo", "Bar", "*"])

      parseD [r|
        instance Nat {
          func is_zero(self): bool {
            match self {
              Zero => true,
              Succ(_) => false,
            }
          }
        }
      |]
        `shouldBe` Instance
                     (ConType (Id ["Nat"]))
                     [ Method
                         "is_zero"
                         ( Closure
                           (ArgTypes []
                           [("self", SelfType)]
                           (ConType (Id ["bool"])))
                           ( Procedure
                             [ Match
                                 (Var (Id ["self"]))
                                 [ (PVar (Id ["Zero"]), Lit (BoolLit True))
                                 , ( PApp (PVar (Id ["Succ"])) [PAny]
                                   , Lit (BoolLit False)
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
        (ArgTypes
        []
        [("()", ConType (Id ["unit"]))]
        (ConType (Id ["unit"])))
        (Procedure [FnCall (Var (Id ["println"])) [Lit (StringLit "\"Hello, World!\"")], Unit])
        )

      parseD [r|
        external func println(x: string);
      |] `shouldBe` ExternalFunc "println" (ArgTypes [] [("x", ConType (Id ["string"]))] (ConType (Id ["unit"])))

      parseD [r|
        func f() {
          for i in foo {
            put(i);
          }
        }
      |] `shouldBe` Func "f" (Closure (ArgTypes [] [("()", ConType (Id ["unit"]))] (ConType (Id ["unit"]))) (
                      Procedure [
                        ForIn "i" (Var (Id ["foo"])) [
                          FnCall (Var (Id ["put"])) [Var (Id ["i"])],
                          Unit
                        ],
                        Unit
                      ]
                    ))

      parseD [r|
        func f() {
          for i in foo(y,z) {
            put(i);
          }
        }
      |] `shouldBe` Func "f" (Closure (ArgTypes [] [("()", ConType (Id ["unit"]))] (ConType (Id ["unit"]))) (
                      Procedure [
                        ForIn "i" (FnCall (Var (Id ["foo"])) [Var (Id ["y"]), Var (Id ["z"])]) [
                          FnCall (Var (Id ["put"])) [Var (Id ["i"])],
                          Unit
                        ],
                        Unit
                      ]
                    ))

      parseD [r|
        func barOf(foo: Foo): int {
          foo.bar
        }
      |] `shouldBe` Func "barOf" (Closure (ArgTypes [] [("foo", ConType (Id ["Foo"]))] (ConType (Id ["int"]))) (Procedure [
          Member (Var (Id ["foo"])) "bar"
        ]))

      parseE [r|
        match s {
          10 => e1,
          foo => e2,
          Const(_, y) => y,
        }
      |] `shouldBe` Match (Var (Id ["s"])) [
          (PLit (IntLit 10), Var (Id ["e1"])),
          (PVar (Id ["foo"]), Var (Id ["e2"])),
          (PApp (PVar (Id ["Const"])) [PAny, PVar (Id ["y"])], Var (Id ["y"]))
        ]
