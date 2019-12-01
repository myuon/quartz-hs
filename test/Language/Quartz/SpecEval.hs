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
      parseE [r| func (a: string): string { a } |] `evaluatedToBe` parseE [r| func (a: string): string { a } |]

      parseE [r| { let id = func <A>(a: A): A { a }; id(1000) } |] `evaluatedToBe` parseE [r| 1000 |]

      parseE [r| { let id = func <A>(a: A): A { a }; id("hello") } |] `evaluatedToBe` parseE [r| "hello" |]

      parseE [r| { let a = 10; a } |] `evaluatedToBe` parseE [r| 10 |]

      parseE [r|
        {
          let f = func (a: string): string { a };
          let z = 10;
          f(z)
        }
      |] `evaluatedToBe` parseE [r| 10 |]

      parseE [r|
        {
          let f = func (a: int, b: int): int { b };
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
        if true { 0 } else { 1 }
      |] `evaluatedToBe` parseE [r| 0 |]

      parseE [r|
        if false { 0 } else if true { 1 } else { 2 }
      |] `evaluatedToBe` parseE [r| 1 |]
