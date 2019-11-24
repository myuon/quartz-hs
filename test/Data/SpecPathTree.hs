module Data.SpecPathTree where

import qualified Data.PathTree as PathTree
import Test.Tasty.Hspec hiding (Failure, Success)

spec_PathTree :: Spec
spec_PathTree = do
  describe "PathTree" $ do
    it "should operate" $ do
      PathTree.find (PathTree.insert ["foo"] 10 PathTree.empty) ["foo"]
        `shouldBe` Just 10

      PathTree.find (PathTree.insert ["foo", "bar", "baz"] 10 PathTree.empty)
                    ["foo", "bar", "baz"]
        `shouldBe` Just 10

      PathTree.find
          ( PathTree.insert ["foo"] 10
          $ PathTree.insert ["foo"] 0
          $ PathTree.empty
          )
          ["foo"]
        `shouldBe` Just 10
