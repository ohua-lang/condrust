module Integrations.Python.TestCases.Basic where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Setup
import qualified Integrations.Python.CodeSamples.TestDataInput as Input
import qualified Integrations.Python.CodeSamples.TestDataOutput as Expect


spec :: Spec
spec =
    describe "Basics" $ do
        it "Simple function call" $
            (showCode "Compiled: " =<< compileCode Input.callAFunction) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callAFunction
                compiled `shouldBe` expected)

        it "Assignment no Return" $
            (showCode "Compiled: " =<< compileCode Input.noReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.noReturn
                compiled `shouldBe` expected)
        
        it "Only 'return' should be the same as no 'return' " $
            (showCode "Compiled: " =<< compileCode Input.emptyReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" =<< compileCode Input.noReturn
                compiled `shouldBe` expected)

        it "Only 'return' should be the same as 'return None' " $
            (showCode "Compiled: " =<<  compileCode Input.emptyReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" =<< compileCode Input.noneReturn
                compiled `shouldBe` expected)
                
        it "Assignment binary Operation of integer literals" $
            (showCode "Compiled: " =<< compileCode Input.assignBinOp) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOp
                compiled `shouldBe` expected)
        
        it "Assignment binary Operation of boolean literals" $
            (showCode "Compiled: " =<< compileCode Input.assignBools) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBools
                compiled `shouldBe` expected) 
        
        it "Assignment binary Operation of vars" $
            (showCode "Compiled: " =<< compileCode Input.assignBinOpChained) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOpChained
                compiled `shouldBe` expected)

        it "Assignment and return a variable" $
            (showCode "Compiled: " =<< compileCode Input.varReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.varReturn
                compiled `shouldBe` expected)     

        it "return function call" $
            (showCode "Compiled: " =<< compileCode Input.onlyReturnFunCall) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.onlyReturnFunCall
                compiled `shouldBe` expected) 

        it "Chained Assignment" $
            (showCode "Compiled: " =<< compileCode Input.chainedAssignment) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.chainedAssignment
                compiled `shouldBe` expected)
        
        it "Nested function calls" $
            (showCode "Compiled: " =<< compileCode Input.nestedCompose) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.nestedCompose
                compiled `shouldBe` expected) 

        it "Algo with params" $
            (showCode "Compiled: " =<< compileCode Input.algoWithParams) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.algoWithParams
                compiled `shouldBe` expected) 

        it "Apply ĺambda expr" $
            (showCode "Compiled: " =<< compileCode Input.applyLambdaExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.applyLambdaExpr
                compiled `shouldBe` expected) 
        
        it "Assign ĺambda expr" $
            (showCode "Compiled: " =<< compileCode Input.assignLambdaExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignLambdaExpr
                compiled `shouldBe` expected) 

        it "Assign empty list" $
            (showCode "Compiled: " =<< compileCode Input.assignEmptyList) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignEmptyList
                compiled `shouldBe` expected) 
        
        it "Assign list with elements" $
            (showCode "Compiled: " =<< compileCode Input.assignList) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignList
                compiled `shouldBe` expected) 

        it "Assign empty dict" $
            (showCode "Compiled: " =<< compileCode Input.assignEmptyDict) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignEmptyDict
                compiled `shouldBe` expected) 


        it "Assign dict with elements" $
            (showCode "Compiled: " =<< compileCode Input.assignDict) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignDict
                compiled `shouldBe` expected) 

        it "Assign set with elements" $
            (showCode "Compiled: " =<< compileCode Input.assignSet) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignSet
                compiled `shouldBe` expected) 
        
        it "Tuple as argument" $
            (showCode "Compiled: " =<< compileCode Input.tupleArgumentCall) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tupleArgumentCall
                compiled `shouldBe` expected)

        it "Assign to subscript" $
            (showCode "Compiled: " =<< compileCode Input.assignToSubscript) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignToSubscript
                compiled `shouldBe` expected)
               
        it "Assign subscript to var" $
            (showCode "Compiled: " =<< compileCode Input.assignSubscript) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignSubscript
                compiled `shouldBe` expected)
        
        
        {-
        -- Fails until I implement augmented assignments
        it "Assignment augmented" $
            (showCode "Compiled: " =<< compileCode Input.assignAugmented) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignAugmented
                compiled `shouldBe` expected)
        -}
