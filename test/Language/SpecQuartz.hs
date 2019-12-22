{-# LANGUAGE QuasiQuotes #-}
module Language.SpecQuartz where

import Control.Monad.State
import Control.Error
import Language.Quartz
import Language.Quartz.Lexer (AlexPosn)
import Language.Quartz.AST
import Test.Tasty.Hspec hiding (Failure, Success)
import qualified Data.Map as M
import Text.RawString.QQ

parseE = either error id . parseExpr

evalETo r1 r2 = do
  result <- runExpr r1
  case result of
    Right v   -> v `shouldBe` r2
    Left  err -> fail $ show err

evalDTo r1 r2 = do
  result <- runModule r1
  case result of
    Right v   -> v `shouldBe` r2
    Left  err -> fail $ show err

spec_quartz :: Spec
spec_quartz = do
  describe "quartz" $ do
    specify "let a = 10; a" $
      [r| { let a = 10; a } |] `evalETo` parseE [r| 10 |]
    specify "id(1000)" $ [r| { let id = <A>(a: A): A -> a; id(1000) } |] `evalETo` parseE [r| 1000 |]
    specify "id(\"hello\")" $ [r| { let id = <A>(a: A): A -> a; id("hello") } |] `evalETo` parseE [r| "hello" |]
    specify "10" $ [r| 10 |] `evalETo` parseE [r| 10 |]
    specify "(a) -> a" $ [r| (a: string): string -> { a } |] `evalETo` parseE [r| (a: string): string -> { a } |]
    specify "(\\a. a)(10)" $ [r|
      {
        let f = <A>(a: A): A -> { a };
        let z = 10;
        f(z)
      }
    |] `evalETo` parseE [r| 10 |]
    specify "(\\a b. b)(10, 20)" $ [r|
      {
        let f = (a: int, b: int): int -> { b };
        let z = 10;
        f(z, 20)
      }
    |] `evalETo` parseE [r| 20 |]

    specify "id(10) in main" $ [r|
      func id(x: int): int {
        x
      }

      func main(): int {
        id(10)
      }
    |] `evalDTo` parseE [r| 10 |]

    specify "array indexing" $ [r|
      {
        let a = [1,2,3,4];
        a[2]
      }
    |] `evalETo` parseE [r| 3 |]

    specify "if" $ [r|
      if {
        true => 0,
        true => 1,
      }
    |] `evalETo` parseE [r| 0 |]

    specify "if with overlapping conditions" $ [r|
      if {
        false => 0,
        true => 1,
        true => 2,
      }
    |] `evalETo` parseE [r| 1 |]

    specify "if with expr condition" $ [r|
      if {
        0 == 1 => 0,
        true => 1,
      }
    |] `evalETo` parseE [r| 1 |]

    specify "record" $ [r|
      record P {
        x: int,
        y: int,
      }

      func main(): int {
        let p = P {
          x: 10,
          y: 20,
        };

        p.x
      }
    |] `evalDTo` parseE [r| 10 |]

    specify "enum" $ [r|
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
    |] `evalDTo` parseE [r| "#f00" |]

    specify "Nat type using enum" $ [r|
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
          Nat::Zero => Nat::Zero,
        }
      }

      func main(): Nat {
        pred(two())
      }
    |] `evalDTo` EnumOf (Id ["Nat", "Succ"]) [EnumOf (Id ["Nat", "Zero"]) []]

    specify "trait" $ [r|
      enum Nat {
        Zero,
        Succ(Nat),
      }

      trait Foo {
        func is_zero(self): bool;
      }

      instance Foo for Nat {
        func is_zero(self): bool {
          match self {
            Nat::Zero => true,
            Nat::Succ(_) => false,
          }
        }
      }

      func main(): bool {
        Nat::Zero.is_zero()
      }
    |] `evalDTo` EnumOf (Id ["Nat", "Succ"]) [EnumOf (Id ["Nat", "Zero"]) []]
