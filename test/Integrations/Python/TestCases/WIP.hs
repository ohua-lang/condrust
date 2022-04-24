module Integrations.Python.TestCases.WIP where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.PythonSetup
import qualified Integrations.Python.PythonCodeSamples.TestDataInput as Input
import qualified Integrations.Python.PythonCodeSamples.TestDataOutput as Expect

import qualified Integrations.Python.PythonCodeSamples.BenchInput as BIn
import qualified Integrations.Python.PythonCodeSamples.BenchOutput as BExpect




spec :: Spec
spec =
    describe "WIP Tests" $ do
        it "Simple function call" $
            (showCode "Compiled: " =<< compileCode Input.callAFunction) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callAFunction
                compiled `shouldBe` expected)
        it "GlobRef" $
            (showCode "Compiled: " =<< compileCode Input.globalRef) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callAFunction
                compiled `shouldBe` expected)

        {-

        it "Assignment binary Operation of integer literals" $
            (showCode "Compiled: " =<< compileCode Input.assignBinOp) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOp
                compiled `shouldBe` expected)
        it "Accept keyword and default args" $
            (showCode "Compiled: " =<< compileCode Input.argsAndKwargs) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.argsAndKwargs
                compiled `shouldBe` expected)
        

        it "Two Algos" $
            (showCode "Compiled: " =<< compileCode Input.twoAlgos) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callMethod
                compiled `shouldBe` expected)

        
        it "Conditional Expression with literals" $
            (showCode "Compiled: " =<< compileCode Input.condExprLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprLit
                compiled `shouldBe` expected)  


        it "Tail Recursive with If-Expr" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)
        
        it "Tail Recursive with If-Stmt" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecStmt) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)

        {-Todo: Error : unitFun must only have one output
        it "Multiassignment comma separated" $
            (showCode "Compiled: " =<< compileCode Input.multiAssignment) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multiAssignment
                compiled `shouldBe` expected)
         -}
        

        it "test args" $
            (showCode "Compiled: " =<< compileCode Input.exprArg) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callAFunction
                compiled `shouldBe` expected)
                

    it "ForLoop over iterator" $
            (showCode "Compiled: " =<< compileCode Input.loopIterator) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterator
                compiled `shouldBe` expected)
        

            -}