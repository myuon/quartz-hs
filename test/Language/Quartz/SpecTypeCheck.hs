{-# LANGUAGE QuasiQuotes #-}
module Language.Quartz.SpecTypeCheck where

import           Control.Error
import           Language.Quartz.Lexer                    ( alexScanTokens
                                                          , AlexPosn
                                                          )
import           Language.Quartz.AST
import           Language.Quartz.Parser
import           Language.Quartz.Transform
import           Language.Quartz.TypeCheck
import           Test.Tasty.Hspec                  hiding ( Failure
                                                          , Success
                                                          )
import           Text.RawString.QQ

runTypeCheck r1 r2 = do
  result <- runExceptT $ inferTypeE r1
  case result of
    Right v   -> v `shouldBe` r2
    Left  err -> fail $ show err

check :: String -> IO ()
check s = do
  result <- runExceptT $ runTypeCheckModule $ parseDs s
  either (fail . show) return result
  return ()

parseE =
  either error id . fmap transformVarConTypeE . parserExpr . alexScanTokens
parseD = either error id . fmap transformVarConTypeD . parser . alexScanTokens
parseDs =
  either error id
    . fmap (fmap transformVarConTypeD)
    . parserDecls
    . alexScanTokens

spec_typecheck :: Spec
spec_typecheck = do
  describe "typechecker" $ do
    it "" $ parseE [r| 10 |] `runTypeCheck` ConType (Id ["int"])

    it "" $ parseE [r| array![1,2,3,4] |] `runTypeCheck` AppType
      (ConType (Id ["array"]))
      [ConType (Id ["int"])]

    it "" $ parseE [r| array!["aaa","bbb"][0] |] `runTypeCheck` ConType
      (Id ["string"])

    it "" $ parseE [r| [](x: int): int => { x } |] `runTypeCheck` FnType
      [ConType (Id ["int"])]
      (ConType (Id ["int"]))

    it ""
      $ parseE [r|
        {
          let f = [](x: int): int => x;
          f(10)
        }
      |]
      `runTypeCheck` ConType (Id ["int"])

    it "" $ do
      parseE [r|
        {
          let f = [](x: int): int => x;
          f(10);
        }
      |]
        `runTypeCheck` ConType (Id ["unit"])

    it "" $ do
      parseE [r|
        {
          let f = [](): int => 10;
          f
        }
      |]
        `runTypeCheck` FnType [] (ConType (Id ["int"]))

    it "" $ do
      parseE [r|
        {
          let f = [A](x: A): A => x;
          let x = 10;
          f(x) == 20
        }
      |]
        `runTypeCheck` ConType (Id ["bool"])

    it "" $ do
      parseE [r|
        if {
          0 == 1 => "true",
          true => "false",
        }
      |]
        `runTypeCheck` ConType (Id ["string"])

    it "" $ do
      check [r|
        record Foo {
          bar: int,
        }

        func barOf(foo: Foo): int {
          foo.bar
        }
      |]

    it "" $ do
      check [r|
        record P {
          x: int,
          y: int
        }

        record Foo {
          bar: int,
          baz: string,
          quux: P
        }

        func hoge(b: string, n: int): Foo {
          Foo {
            bar: n,
            baz: b,
            quux: P {
              x: n,
              y: 20,
            },
          }
        }
      |]

    it "" $ do
      check [r|
        enum Color {
          Red,
          Blue,
          Yellow,
        }

        func red(): Color {
          Color::Red
        }

        func color_code(c: Color): string {
          match c {
            Red => "#f00",
            Blue => "#00f",
            Yellow => "#ff0",
          }
        }
      |]

    it "" $ do
      check [r|
        enum Nat {
          Zero,
          Succ(Nat),
        }

        func zero(): Nat {
          Nat::Zero
        }

        func two(): Nat {
          Nat::Succ(Nat::Succ(Nat::Zero))
        }

        func pred(n: Nat): Nat {
          match n {
            Nat::Succ(m) => m,
            _ => n,
          }
        }
      |]

    it "" $ do
      check [r|
        enum Tree[T] {
          Empty,
          Node(Tree[T], T, Tree[T]),
        }

        func node[T](left: Tree[T], value: T, right: Tree[T]): Tree[T] {
          Tree::Node(left, value, right)
        }

        func get_value[T](tree: Tree[T]): T {
          match tree {
            Tree::Node(_, v, _) => v
          }
        }
      |]

    it "" $ do
      check [r|
        record Pair[X,Y] {
          proj1: X,
          proj2: Y,
        }

        func pair[S,T](x: S, y: T): Pair[S,T] {
          Pair {
            proj1: x,
            proj2: y,
          }
        }
      |]

    it "" $ do
      check [r|
        interface Hoge[T] {
          func doSth(x: T);
          func get(self): T;
        }

        derive Hoge[T] for int {
          func get(self): T {
            100
          }
        }
      |]
