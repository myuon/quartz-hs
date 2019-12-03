{-# LANGUAGE QuasiQuotes #-}
module Language.Quartz.SpecTypeCheck where

import Control.Error
import Language.Quartz.Lexer (alexScanTokens)
import Language.Quartz.AST
import Language.Quartz.Parser
import Language.Quartz.TypeCheck
import Test.Tasty.Hspec hiding (Failure, Success)
import Text.RawString.QQ

runTypeCheck r1 r2 = do
  result <- runExceptT $ runTypeCheckExpr r1
  case result of
    Right v   -> v `shouldBe` r2
    Left  err -> fail $ show err

check s = do
  result <- runExceptT $ runTypeCheckModule $ parseDs s
  either (fail . show) return result

parseE = either error id . parserExpr . alexScanTokens
parseD = either error id . parser . alexScanTokens
parseDs = either error id . parserDecls . alexScanTokens

spec_typecheck :: Spec
spec_typecheck = do
  describe "typechecker" $ do
    it "should typecheck" $ do
      parseE [r| 10 |] `runTypeCheck` ConType (Id ["int"])

      parseE [r| [1,2,3,4] |] `runTypeCheck` AppType (ConType (Id ["array"])) [ConType (Id ["int"])]

      parseE [r| ["aaa","bbb"][0] |] `runTypeCheck` ConType (Id ["string"])

      parseE [r| func (x: int): int { x } |]
        `runTypeCheck` (ConType (Id ["int"]) `ArrowType` ConType (Id ["int"]))

      parseE [r|
        {
          let f = func (x: int): int { x };
          f(10)
        }
      |]
        `runTypeCheck` ConType (Id ["int"])

      parseE [r|
        {
          let f = func (x: int): int { x };
          f(10);
        }
      |]
        `runTypeCheck` ConType (Id ["unit"])

      parseE [r|
        {
          let f = func (): int { 10 };
          f
        }
      |]
        `runTypeCheck` (ConType (Id ["unit"]) `ArrowType` ConType (Id ["int"]))

      parseE [r|
        {
          let f = func<A>(x: A): A { x };
          let x = 10;
          f(x) == 20
        }
      |] `runTypeCheck` ConType (Id ["bool"])

      parseE [r|
        if {
          0 == 1 -> "true",
          true -> "false",
        }
      |] `runTypeCheck` ConType (Id ["string"])

      check [r|
        record Foo {
          bar: int,
        }

        func barOf(foo: Foo): int {
          foo.bar
        }
      |]

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
