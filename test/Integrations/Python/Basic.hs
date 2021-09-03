module Integrations.Python.Basic where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "Basics" $ do
        it "Simple function call" $
            (showCode "Compiled: " =<< compileCode Input.callAFunction) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callAFunction
                compiled `shouldBe` expected)

        it "Assignment numeric Literal" $
            (showCode "Compiled: " =<< compileCode Input.assignNumLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignNumLit
                compiled `shouldBe` expected)
        
        it "Assignment binary Operation of literals" $
            (showCode "Compiled: " =<< compileCode Input.assignBinOp) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOp
                compiled `shouldBe` expected)
        
        it "Assignment binary Operation of vars" $
            (showCode "Compiled: " =<< compileCode Input.assignBinOpChained) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOpChained
                compiled `shouldBe` expected)

        {-
        -- currently errors, augAss not implemented
        it "Assignment augmented" $
            (showCode "Compiled: " =<< compileCode Input.assignAugmented) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignAugmented
                compiled `shouldBe` expected)
        -}
        it "Assignment no Return" $
            (showCode "Compiled: " =<< compileCode Input.noReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.noReturn
                compiled `shouldBe` expected)
        
        it "Assignment and return" $
            (showCode "Compiled: " =<< compileCode Input.emptyReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.emptyReturn
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
        
        it "Assignment and return None" $
            (showCode "Compiled: " =<< compileCode Input.noneReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.noneReturn
                compiled `shouldBe` expected)
        
        it "Expression no Return" $
            (showCode "Compiled: " =<< compileCode Input.exprNoReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.exprNoReturn
                compiled `shouldBe` expected)

        --Invalid Python: fails, because wrong channels are created vs used
        it "Multiassignment no Return" $
            (showCode "Compiled: " =<< compileCode Input.multiAssignment) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multiAssignment
                compiled `shouldBe` expected)

        it "Chained Assignment" $
            (showCode "Compiled: " =<< compileCode Input.chainedAssignment) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.chainedAssignment
                compiled `shouldBe` expected)

        --Invalid Python: fails, because wrong channels are created vs used
        it "Assignments, Call function, Return Var" $
            (showCode "Compiled: " =<< compileCode Input.assignmentCallReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignmentCallReturn
                compiled `shouldBe` expected)

        --Invalid Python: fails, because wrong channels are created vs used
        it "Assignments, Call function, Assign,  Return Var" $
            (showCode "Compiled: " =<< compileCode Input.assignCallAssignReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignCallAssignReturn
                compiled `shouldBe` expected)

        it "Nested function calls" $
            (showCode "Compiled: " =<< compileCode Input.nestedCompose) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.nestedCompose
                compiled `shouldBe` expected) 
                