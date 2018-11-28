{-# LANGUAGE QuasiQuotes #-}

module CodeGenSpec where

import Universum

import Test.Hspec

import One4All.Quote

codeGenSpec :: Spec
codeGenSpec = do
  describe "Testing code generation" $
    it "simple expression" $ genSimpleExpr `shouldBe` expectedSimpleExpr

genSimpleExpr = undefined

expectedSimpleExpr = [expr| let task1 = createTask in task1 |]
