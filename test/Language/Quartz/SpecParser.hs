{-# LANGUAGE QuasiQuotes #-}
module Language.Quartz.SpecParser where

import           Language.Quartz.Lexer                    ( alexScanTokens
                                                          , Lexeme
                                                          , AlexPosn(AlexPn)
                                                          )
import           Language.Quartz.AST
import           Language.Quartz.Parser
import           Language.Quartz.Transform
import           Test.Tasty.Hspec                  hiding ( Failure
                                                          , Success
                                                          )
import           Text.RawString.QQ

parseE =
  either error id
    . fmap (transformIgnorePosnE . transformVarConTypeE)
    . parserExpr
    . alexScanTokens
parseD =
  either error id
    . fmap (transformIgnorePosnD . transformVarConTypeD)
    . parser
    . alexScanTokens
parseDs =
  either error id
    . fmap (map (transformIgnorePosnD . transformVarConTypeD))
    . parserDecls
    . alexScanTokens

spec_parser :: Spec
spec_parser = do
  describe "parser" $ do
    it "" $ do
      parseE "xxx" `shouldBe` Var Nothing (Id ["xxx"])

    it "" $ do
      parseE "10" `shouldBe` Lit (IntLit 10)

    it "" $ do
      parseE [r| "aaa" |] `shouldBe` Lit (StringLit "aaa")

    it "" $ do
      parseE [r| "あああ" |] `shouldBe` Lit (StringLit "あああ")

    it "" $ do
      parseE [r| x.y.z.w |] `shouldBe` Member
        (Member (Member (Var Nothing (Id ["x"])) "y") "z")
        "w"

    it "" $ do
      parseE "foo(x,y,z)" `shouldBe` FnCall
        (Var Nothing (Id ["foo"]))
        [Var Nothing (Id ["x"]), Var Nothing (Id ["y"]), Var Nothing (Id ["z"])]

    it "" $ do
      parseE "x.foo(y,z)" `shouldBe` FnCall
        (Member (Var Nothing (Id ["x"])) "foo")
        [Var Nothing (Id ["y"]), Var Nothing (Id ["z"])]

    it "" $ do
      parseE "a.b.c" `shouldBe` Member (Member (Var Nothing (Id ["a"])) "b") "c"

    it "" $ do
      parseE "array![1,2,3,4]" `shouldBe` ArrayLit
        [Lit (IntLit 1), Lit (IntLit 2), Lit (IntLit 3), Lit (IntLit 4)]

    it "" $ do
      parseE "h(f)[g(1)]" `shouldBe` IndexArray
        (FnCall (Var Nothing (Id ["h"])) [Var Nothing (Id ["f"])])
        (FnCall (Var Nothing (Id ["g"])) [Lit (IntLit 1)])

    it "" $ do
      parseE [r|
        if {
          b1 => e1,
          b2 => e2,
          true => e3,
        }
      |]
        `shouldBe` If
                     [ (Var Nothing (Id ["b1"]), Var Nothing (Id ["e1"]))
                     , (Var Nothing (Id ["b2"]), Var Nothing (Id ["e2"]))
                     , (Lit (BoolLit True)     , Var Nothing (Id ["e3"]))
                     ]

    it "" $ do
      parseE [r|
        if {
          0 == 1 => "true",
          true => "false",
        }
      |]
        `shouldBe` If
                     [ ( Op Eq (Lit (IntLit 0)) (Lit (IntLit 1))
                       , Lit (StringLit "true")
                       )
                     , (Lit (BoolLit True), Lit (StringLit "false"))
                     ]

    it "" $ do
      parseE "[](a: string): string => { a }" `shouldBe` ClosureE
        (Closure
          (FuncType []
                    (ArgType False False [("a", ConType (Id ["string"]))])
                    (ConType (Id ["string"]))
          )
          (Procedure [(Nothing, Var Nothing (Id ["a"]))])
        )

    it "" $ do
      parseE "[A](a: A): A => a" `shouldBe` ClosureE
        (Closure
          (FuncType ["A"]
                    (ArgType False False [("a", VarType "A")])
                    (VarType "A")
          )
          (Var Nothing (Id ["a"]))
        )

    it "" $ do
      parseE [r|
        Pos {
          x: 10,
          y: "foo"
        }
      |]
        `shouldBe` RecordOf
                     "Pos"
                     [("x", Lit (IntLit 10)), ("y", Lit (StringLit "foo"))]

    it "" $ do
      parseE "[](a: int, b: int, c: int) => { let z = sum(a,b,c); z }"
        `shouldBe` ClosureE
                     (Closure
                       (FuncType
                         []
                         (ArgType
                           False
                           False
                           [ ("a", ConType (Id ["int"]))
                           , ("b", ConType (Id ["int"]))
                           , ("c", ConType (Id ["int"]))
                           ]
                         )
                         (ConType (Id ["unit"]))
                       )
                       (Procedure
                         [ ( Nothing
                           , Stmt $ Let
                             (Id ["z"])
                             (FnCall
                               (Var Nothing (Id ["sum"]))
                               [ Var Nothing (Id ["a"])
                               , Var Nothing (Id ["b"])
                               , Var Nothing (Id ["c"])
                               ]
                             )
                           )
                         , (Nothing, Var Nothing (Id ["z"]))
                         ]
                       )
                     )

    it "" $ do
      parseE [r|
        {
          let f = [](): int => { 10 };
          f
        }
      |]
        `shouldBe` Procedure
                     [ ( Nothing
                       , Stmt $ Let
                         (Id ["f"])
                         (ClosureE
                           (Closure
                             (FuncType []
                                       (ArgType False False [])
                                       (ConType (Id ["int"]))
                             )
                             (Procedure [(Nothing, Lit (IntLit 10))])
                           )
                         )
                       )
                     , (Nothing, Var Nothing (Id ["f"]))
                     ]

    it "" $ do
      parseE [r|
        if {
          (x == y) => e1,
        }
      |]
        `shouldBe` If
                     [ ( Op Eq (Var Nothing (Id ["x"])) (Var Nothing (Id ["y"]))
                       , Var Nothing (Id ["e1"])
                       )
                     ]

    it "" $ do
      parseD "func id[A](x: A): A { let y = x; y }" `shouldBe` Func
        "id"
        (Closure
          (FuncType ["A"]
                    (ArgType False False [("x", VarType "A")])
                    (VarType "A")
          )
          (Procedure
            [ (Nothing, Stmt $ Let (Id ["y"]) (Var Nothing (Id ["x"])))
            , (Nothing, Var Nothing (Id ["y"]))
            ]
          )
        )

    it "" $ do
      parseD "enum Nat { Zero, Succ(Nat) }" `shouldBe` Enum
        "Nat"
        []
        [EnumField "Zero" [], EnumField "Succ" [ConType (Id ["Nat"])]]

    it "" $ do
      parseD "enum Node[T] { Node(T, int, T) }" `shouldBe` Enum
        "Node"
        ["T"]
        [EnumField "Node" [VarType "T", ConType (Id ["int"]), VarType "T"]]

    it "" $ do
      parseD "record User { user_id: string, age: int, }" `shouldBe` Record
        "User"
        []
        [ RecordField "user_id" (ConType (Id ["string"]))
        , RecordField "age"     (ConType (Id ["int"]))
        ]

    it "" $ do
      parseD "record Pair[X,Y] { proj1: X, proj2: Y, }" `shouldBe` Record
        "Pair"
        ["X", "Y"]
        [RecordField "proj1" (VarType "X"), RecordField "proj2" (VarType "Y")]

    {-
    it "" $ do
      parseD "open List::Foo::Bar::*;"
        `shouldBe` OpenD (Id ["List", "Foo", "Bar", "*"])
    -}

    it "" $ do
      parseD [r|
        derive Nat {
          func is_zero(self): bool {
            match self {
              Zero => true,
              Succ(_) => false,
            }
          }
        }
      |]
        `shouldBe` Derive
                     "Nat"
                     []
                     Nothing
                     [ Func
                         "is_zero"
                         (Closure
                           (FuncType []
                                     (ArgType False True [])
                                     (ConType (Id ["bool"]))
                           )
                           (Procedure
                             [ ( Nothing
                               , Match
                                 (Self SelfType)
                                 [ (PVar (Id ["Zero"]), Lit (BoolLit True))
                                 , ( PApp (PVar (Id ["Succ"])) [PAny]
                                   , Lit (BoolLit False)
                                   )
                                 ]
                               )
                             ]
                           )
                         )
                     ]

    it "" $ do
      parseD [r|
        func main() {
          println("Hello, World!");
        }
      |]
        `shouldBe` Func
                     "main"
                     (Closure
                       (FuncType []
                                 (ArgType False False [])
                                 (ConType (Id ["unit"]))
                       )
                       (Procedure
                         [ ( Nothing
                           , Stmt $ FnCall (Var Nothing (Id ["println"]))
                                           [Lit (StringLit "Hello, World!")]
                           )
                         ]
                       )
                     )

    it "" $ do
      parseD [r|
        external func println(x: string);
      |]
        `shouldBe` ExternalFunc
                     "println"
                     (FuncType
                       []
                       (ArgType False False [("x", ConType (Id ["string"]))])
                       (ConType (Id ["unit"]))
                     )

    it "" $ do
      parseD [r|
        func f() {
          for i in foo {
            put(i);
          }
        }
      |]
        `shouldBe` Func
                     "f"
                     (Closure
                       (FuncType []
                                 (ArgType False False [])
                                 (ConType (Id ["unit"]))
                       )
                       (Procedure
                         [ ( Nothing
                           , ForIn
                             "i"
                             (Var Nothing (Id ["foo"]))
                             [ ( Nothing
                               , Stmt $ FnCall (Var Nothing (Id ["put"]))
                                               [Var Nothing (Id ["i"])]
                               )
                             ]
                           )
                         ]
                       )
                     )

    it "" $ do
      parseD [r|
        func f() {
          for i in foo(y,z) {
            put(i);
          }
        }
      |]
        `shouldBe` Func
                     "f"
                     (Closure
                       (FuncType []
                                 (ArgType False False [])
                                 (ConType (Id ["unit"]))
                       )
                       (Procedure
                         [ ( Nothing
                           , ForIn
                             "i"
                             (FnCall
                               (Var Nothing (Id ["foo"]))
                               [Var Nothing (Id ["y"]), Var Nothing (Id ["z"])]
                             )
                             [ ( Nothing
                               , Stmt $ FnCall (Var Nothing (Id ["put"]))
                                               [Var Nothing (Id ["i"])]
                               )
                             ]
                           )
                         ]
                       )
                     )

    it "" $ do
      parseD [r|
        func barOf(foo: Foo): int {
          foo.bar
        }
      |]
        `shouldBe` Func
                     "barOf"
                     (Closure
                       (FuncType
                         []
                         (ArgType False False [("foo", ConType (Id ["Foo"]))])
                         (ConType (Id ["int"]))
                       )
                       (Procedure
                         [(Nothing, Member (Var Nothing (Id ["foo"])) "bar")]
                       )
                     )

    it "" $ do
      parseE [r|
        match s {
          10 => e1,
          foo => e2,
          Const(_, y) => y,
        }
      |]
        `shouldBe` Match
                     (Var Nothing (Id ["s"]))
                     [ (PLit (IntLit 10) , Var Nothing (Id ["e1"]))
                     , (PVar (Id ["foo"]), Var Nothing (Id ["e2"]))
                     , ( PApp (PVar (Id ["Const"])) [PAny, PVar (Id ["y"])]
                       , Var Nothing (Id ["y"])
                       )
                     ]

    it "" $ do
      parseD [r|
        func snoc[T](xs: List[T], x: T): List[T] {
        }
      |]
        `shouldBe` Func
                     "snoc"
                     (Closure
                       (FuncType
                         ["T"]
                         (ArgType
                           False
                           False
                           [ ( "xs"
                             , AppType (ConType (Id ["List"])) [VarType "T"]
                             )
                           , ("x", VarType "T")
                           ]
                         )
                         (AppType (ConType (Id ["List"])) [VarType "T"])
                       )
                       (Procedure [])
                     )

    it "" $ do
      parseD [r|
        interface IState {
          func get[T](self, i: int): T;
          func put[T](self, i: int, val: T);
        }
      |]
        `shouldBe` Interface
                     "IState"
                     []
                     [ (,)
                       "get"
                       (FuncType
                         ["T"]
                         (ArgType False True [("i", ConType (Id ["int"]))])
                         (VarType "T")
                       )
                     , (,)
                       "put"
                       (FuncType
                         ["T"]
                         (ArgType
                           False
                           True
                           [("i", ConType (Id ["int"])), ("val", VarType "T")]
                         )
                         (ConType (Id ["unit"]))
                       )
                     ]

    it "" $ do
      parseD [r|
        derive IState for array[int] {
          func get[T](self, i: int): T {
            self[i]
          }
        }
      |]
        `shouldBe` Derive
                     "IState"
                     []
                     (Just
                       (AppType (ConType (Id ["array"])) [ConType (Id ["int"])])
                     )
                     [ Func
                         "get"
                         (Closure
                           (FuncType
                             ["T"]
                             (ArgType False True [("i", ConType (Id ["int"]))])
                             (VarType "T")
                           )
                           (Procedure
                             [ ( Nothing
                               , IndexArray (Self SelfType)
                                            (Var Nothing (Id ["i"]))
                               )
                             ]
                           )
                         )
                     ]
