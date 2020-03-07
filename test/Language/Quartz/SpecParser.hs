{-# LANGUAGE QuasiQuotes #-}
module Language.Quartz.SpecParser where

import           Language.Quartz.AST
import           Language.Quartz.Lexer
import           Language.Quartz.Parser
import           Language.Quartz.Transform
import           Test.Tasty.Hspec                  hiding ( Failure
                                                          , Success
                                                          )
import           Text.RawString.QQ

shouldNoError a = a `shouldBe` a

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
      shouldNoError $ parseE "xxx"
{-
    it "" $ do
      shouldNoError $ parseE "10"

    it "" $ do
      shouldNoError $ parseE [r| "aaa" |]

    it "" $ do
      shouldNoError $ parseE [r| "あああ" |]

    it "" $ do
      shouldNoError $ parseE [r| x.y.z.w |]

    it "" $ do
      shouldNoError $ parseE "foo(x,y,z)"

    it "" $ do
      shouldNoError $ parseE "x.foo(y,z)"

    it "" $ do
      shouldNoError $ parseE "a.b.c"

    it "" $ do
      shouldNoError $ parseE "array![1,2,3,4]"

    it "" $ do
      shouldNoError $ parseE "h(f)[g(1)]"

    it "" $ do
      shouldNoError $ parseE [r|
        if {
          b1 => e1,
          b2 => e2,
          true => e3,
        }
      |]

    it "" $ do
      shouldNoError $ parseE [r|
        if {
          0 == 1 => "true",
          true => "false",
        }
      |]

    it "" $ do
      shouldNoError $ parseE "[](a: string): string => { a }"

    it "" $ do
      shouldNoError $ parseE "[A](a: A): A => a"

    it "" $ do
      shouldNoError $ parseE [r|
        Pos {
          x: 10,
          y: "foo"
        }
      |]

    it "" $ do
      shouldNoError
        $ parseE "[](a: int, b: int, c: int) => { let z = sum(a,b,c); z }"

    it "" $ do
      shouldNoError $ parseE [r|
        {
          let f = [](): int => { 10 };
          f
        }
      |]

    it "" $ do
      shouldNoError $ parseE [r|
        if {
          (x == y) => e1,
        }
      |]

    it "" $ do
      shouldNoError $ parseE [r|
        {
          [](s) => s
        }
      |]

    it "" $ do
      shouldNoError $ parseD "func id[A](x: A): A { let y = x; y }"

    it "" $ do
      shouldNoError $ parseD "enum Nat { Zero, Succ(Nat) }"

    it "" $ do
      shouldNoError $ parseD "enum Node[T] { Node(T, int, T) }"

    it "" $ do
      shouldNoError $ parseD "record User { user_id: string, age: int, }"

    it "" $ do
      shouldNoError $ parseD "record Pair[X,Y] { proj1: X, proj2: Y, }"

    it "" $ do
      shouldNoError $ parseD "open List::Foo::Bar::*;"

    it "" $ do
      shouldNoError $ parseD [r|
        derive Nat {
          func is_zero(self): bool {
            match self {
              Zero => true,
              Succ(_) => false,
            }
          }
        }
      |]

    it "" $ do
      shouldNoError $ parseD [r|
        func main() {
          println("Hello, World!");
        }
      |]

    it "" $ do
      shouldNoError $ parseD [r|
        external func println(x: string);
      |]

    it "" $ do
      shouldNoError $ parseD [r|
        func f() {
          for i in foo {
            put(i);
          }
        }
      |]

    it "" $ do
      shouldNoError $ parseD [r|
        func f() {
          for i in foo(y,z) {
            put(i);
          }
        }
      |]

    it "" $ do
      shouldNoError $ parseD [r|
        func barOf(foo: Foo): int {
          foo.bar
        }
      |]

    it "" $ do
      shouldNoError $ parseE [r|
        match s {
          10 => e1,
          foo => e2,
          Const(_, y) => y,
        }
      |]

    it "" $ do
      shouldNoError $ parseD [r|
        func snoc[T](xs: List[T], x: T): List[T] {
        }
      |]

    it "" $ do
      shouldNoError $ parseD [r|
        interface IState {
          func get[T](self, i: int): T;
          func put[T](self, i: int, val: T);
        }
      |]

    it "" $ do
      shouldNoError $ parseD [r|
        derive IState for array[int] {
          func get[T](self, i: int): T {
            self[i]
          }
        }
      |]
-}
