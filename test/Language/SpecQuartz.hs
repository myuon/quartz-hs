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
    specify "id(\"hello\")" $ [r| { let id = <A>(a: A): A -> a; id("hello") } |] `evalETo` Lit (StringLit "hello")
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

      interface Foo {
        func is_zero(self): bool;
        func identity(self): self;
      }

      derive Foo for Nat {
        func is_zero(self): bool {
          match self {
            Nat::Zero => true,
            Nat::Succ(_) => false,
          }
        }

        func identity(self): self {
          self
        }
      }

      func main(): bool {
        Nat::Zero.identity().is_zero()
      }
    |] `evalDTo` Lit (BoolLit True)

    specify "factorial" $ [r|
      func factorial(n: int): int {
        if {
          n == 0 => { 1 },
          true => { n * factorial(n - 1) },
        }
      }

      func main(): int {
        factorial(10)
      }
    |] `evalDTo` Lit (IntLit 3628800)

    specify "array_literal" $ [r|
      {
        let id = <A>(x: A): A -> x;
        [id(1)][0]
      }
    |] `evalETo` Lit (IntLit 1)

    specify "associated methods" $ [r|
      enum Nat {
        Zero,
        Succ(Nat),
      }

      derive Nat {
        func is_zero(self): bool {
          match self {
            Nat::Zero => true,
            Nat::Succ(_) => false,
          }
        }

        func identity(self): self {
          self
        }
      }

      func main(): bool {
        Nat::Zero.identity().is_zero()
      }
    |] `evalDTo` Lit (BoolLit True)

    specify "int calculation" $ [r|
      ((1 + 2) * 4) - (10 / 2)
    |] `evalETo` Lit (IntLit 7)

    specify "call non-self method" $ [r|
      enum Nat {
        Zero,
        Succ(Nat),
      }

      derive Nat {
        func succ(self): self {
          Nat::Succ(self)
        }

        func two(): self {
          Nat::Zero.succ().succ()
        }
      }

      func main(): self {
        Nat::two()
      }
    |] `evalDTo` EnumOf (Id ["Nat", "Succ"]) [EnumOf (Id ["Nat", "Succ"]) [EnumOf (Id ["Nat", "Zero"]) []]]

    specify "1 <= 200" $ [r|
      1 <= 200
    |] `evalETo` Lit (BoolLit True)

    specify "1 < 200" $ [r|
      1 < 200
    |] `evalETo` Lit (BoolLit True)

    specify "200 >= 1" $ [r|
      200 >= 1
    |] `evalETo` Lit (BoolLit True)

    specify "200 > 1" $ [r|
      200 > 1
    |] `evalETo` Lit (BoolLit True)

    specify "assign with reference" $ [r|
      {
        let v = &10;
        v = 20;
        *v
      }
    |] `evalETo` Lit (IntLit 20)

    specify "assign to variable via reference" $ [r|
      record R {
        x: int,
      }

      func main(): int {
        let r = &R { x: 10 };
        r = R { x: 20 };
        r.x
      }
    |] `evalDTo` Lit (IntLit 20)

    specify "record modification via ref self method" $ [r|
      record R {
        x: int,
      }

      derive R {
        func addX(&self, val: int) {
          self = R { x: self.x + val };
        }
      }

      func main(): int {
        let r = &R { x: 10 };
        r.addX(20);
        r.x
      }
    |] `evalDTo` Lit (IntLit 30)

  describe "stdlib" $ do
    describe "vector" $ do
      specify "push" $ [r|
        {
          let v = vector::new();
          v.push(1);
          v.push(2);
          v.push(3);

          v.get(2)
        }
      |] `evalETo` Lit (IntLit 0)

      specify "grow" $ [r|
        {
          let v = vector::new();
          v.push(1);
          v.push(2);
          v.push(3);
          v.push(4);
          v.push(5);
          v.push(6);

          v.capacity()
        }
      |] `evalETo` Lit (IntLit 10)
