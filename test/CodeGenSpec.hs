{-# LANGUAGE QuasiQuotes #-}

module CodeGenSpec where

import Universum

import Test.Hspec

import Ohua.DFGraph
import Ohua.Types
import One4All.Quote

codeGenSpec :: Spec
codeGenSpec = do
    describe "Testing code generation:" $ do
        it "simple expression" $ genSimpleExpr `shouldBe` expectedSimpleExpr
        it "arc generation" $ genArcExpr `shouldBe` expectedArcExpr
        it "task generation" $ genTasksExpr `shouldBe` expectedTasksExpr

genSimpleExpr = undefined

expectedSimpleExpr = [expr| let task1 = createTask in task1 |]

genArcExpr = undefined

expectedArcExpr =
    [expr| let chan = channel () in
                       let sender = nth 0 chan in
                       let receiver = nth 1 chan in toBeCompleted |]

genTasksExpr = undefined

expectedTasksExpr =
    [expr| let codeTask1 = \() -> let result = some_sfn () in
                         send sender result
                         in
                     let codeTask2 = \() -> let dat = receive receiver in
                         let result = some_other_sfn dat in
                           ()
                         in
     let task1 = task codeTask1 in
       let task2 = task codeTask2 in
          let l = mkList () in
          let l1 = addToList l task1 in
          let l2 = addToList l1 task1 in toBeCompleted |]

genProdCons :: QualifiedBinding -> QualifiedBinding -> Int -> OutGraph
genProdCons prod con out_idx =
    OutGraph
        [Operator 0 prod, Operator 1 con]
        [Arc (Target 1 0) (LocalSource (Target 0 out_idx))]
        (Target 1 (-1))
