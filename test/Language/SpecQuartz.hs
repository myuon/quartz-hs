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

spec_quartz :: Spec
spec_quartz = do
  describe "quartz" $ do
    specify "let a = 10; a" $
      [r| { let a = 10; a } |] `evalETo` parseE [r| 10 |]
    specify "id(1000)" $ [r| { let id = <A>(a: A): A -> a; id(1000) } |] `evalETo` parseE [r| 1000 |]
    specify "id(\"hello\")" $ [r| { let id = <A>(a: A): A -> a; id("hello") } |] `evalETo` parseE [r| "hello" |]
