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
      parseE [r| (a: string): string -> a |] `evaluatedToBe` parseE [r| (a: string): string -> a |]

      parseE [r| { let a = 10; a } |] `evaluatedToBe` parseE [r| 10 |]

      parseE [r|
        {
          let f = (a: string): string -> a;
          let z = 10;
          f(z)
        }
      |] `evaluatedToBe` parseE [r| 10 |]

      parseE [r|
        {
          let f = (a: int, b: int): int -> b;
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
