{-# LANGUAGE QuasiQuotes #-}
module Language.Quartz.SpecEval where

import Control.Monad.State
import Control.Error
import Language.Quartz.Eval
import Language.Quartz.AST
import Language.Quartz.Lexer (alexScanTokens)
import Language.Quartz.Parser
import Test.Tasty.Hspec hiding (Failure, Success)
import qualified Data.Map as M
import Text.RawString.QQ

evaluatedToBe r1 r2 = do
  result <- runExceptT $ runEvalE r1
  case result of
    Right v   -> v `shouldBe` r2
    Left  err -> fail $ show err

runMainResult r1 r2 = do
  result <- runExceptT $ runMain r1
  case result of
    Right v   -> v `shouldBe` r2
    Left  err -> fail $ show err

parseE = either error id . parserExpr . alexScanTokens
parseD = either error id . parser . alexScanTokens
parseDs = either error id . parserDecls . alexScanTokens

spec_evaluate :: Spec
spec_evaluate = do
  describe "evaluate" $ do
    it "should evaluate" $ do
      Lit (IntLit 10) `evaluatedToBe` Lit (IntLit 10)

    it "should parse and eval" $ do
      parseE [r| (a: string): string -> { a } |] `evaluatedToBe` parseE [r| (a: string): string -> { a } |]

      parseE [r| { let id = <A>(a: A): A -> a; id(1000) } |] `evaluatedToBe` parseE [r| 1000 |]

      parseE [r| { let id = <A>(a: A): A -> a; id("hello") } |] `evaluatedToBe` parseE [r| "hello" |]

      parseE [r| { let a = 10; a } |] `evaluatedToBe` parseE [r| 10 |]

      parseE [r|
        {
          let f = (a: string): string -> { a };
          let z = 10;
          f(z)
        }
      |] `evaluatedToBe` parseE [r| 10 |]

      parseE [r|
        {
          let f = (a: int, b: int): int -> { b };
          let z = 10;
          f(z, 20)
        }
      |] `evaluatedToBe` parseE [r| 20 |]

      parseDs [r|
        func id(x: nat): nat {
          x
        }

        func main(): nat {
          id(10)
        }
      |] `runMainResult` parseE [r| 10 |]

      parseE [r|
        {
          let a = [1,2,3,4];
          a[2]
        }
      |] `evaluatedToBe` parseE [r| 3 |]

      parseE [r|
        if {
          true => 0,
          true => 1,
        }
      |] `evaluatedToBe` parseE [r| 0 |]

      parseE [r|
        if {
          false => 0,
          true => 1,
          true => 2,
        }
      |] `evaluatedToBe` parseE [r| 1 |]

      parseE [r|
        if {
          0 == 1 => 0,
          true => 1,
        }
      |] `evaluatedToBe` parseE [r| 1 |]

      parseDs [r|
        record P {
          x: int,
          y: int,
        }

        func main(): nat {
          let p = P {
            x: 10,
            y: 20,
          };

          p.x
        }
      |] `runMainResult` parseE [r| 10 |]

      parseDs [r|
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

        func main(): string {
          color_code(red())
        }
      |] `runMainResult` parseE [r| "#f00" |]

      parseDs [r|
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

        func main(): Nat {
          pred(two())
        }
      |] `runMainResult` parseE [r| Nat::Succ(Nat::Zero) |]
