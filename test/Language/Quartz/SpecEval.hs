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

parseE = either error id . parserExpr . alexScanTokens
parseD = either error id . parser . alexScanTokens

spec_Evaluate :: Spec
spec_Evaluate = do
  describe "evaluate" $ do
    it "should evaluate" $ do
      Lit (IntLit 10) `evaluatedToBe` Lit (IntLit 10)

    it "should parse and eval" $ do
      parseE [r| (a: string): string -> a |] `evaluatedToBe` ClosureE
        ( Closure (VarType "string" `ArrowType` VarType "string")
                  ["a"]
                  (Var (Id ["a"]))
        )

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
