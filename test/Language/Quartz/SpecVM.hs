module Language.Quartz.SpecVM where

import Control.Monad.State
import Control.Error
import Language.Quartz.VM
import Test.Tasty.Hspec hiding (Failure, Success)
import qualified Data.Map as M

evaluatedToBe r1 r2 = do
  result <- runExceptT $ runEvaluate r1
  case result of
    Right v   -> v `shouldBe` r2
    Left  err -> fail $ show err

spec_Evaluate :: Spec
spec_Evaluate = do
  describe "evaluate" $ do
    it "should evaluate" $ do
      Lit (IntLit 10) `evaluatedToBe` Lit (IntLit 10)

      Lam ["a"]            (Var "a") `evaluatedToBe` Lam ["a"] (Var "a")

      App (Lit (IntLit 0)) [Lit (IntLit 1)]
        `evaluatedToBe` App (Lit (IntLit 0)) [Lit (IntLit 1)]

      App (Lam ["a"] (Var "a")) [Lit (IntLit 1)] `evaluatedToBe` Lit (IntLit 1)

      App (Lam ["a", "b", "c"] (Var "a")) [Lit (IntLit 1)]
        `evaluatedToBe` Lam ["b", "c"] (Lit (IntLit 1))

      App (Lam ["a"] (Var "a")) [Lit (IntLit 0), Lit (IntLit 1)]
        `evaluatedToBe` App (Lit (IntLit 0)) [Lit (IntLit 1)]

      App (App (Lam ["a"] (Var "a")) [Lit (IntLit 0)]) []
        `evaluatedToBe` Lit (IntLit 0)

      Let "a" (Lit (IntLit 10)) (Var "a") `evaluatedToBe` Lit (IntLit 10)
