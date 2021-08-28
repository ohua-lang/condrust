module Integrations.Python.Basic where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "Basics" $ do
        it "placeholder test" $ 
          1 `shouldBe` 1
        {-it "Simple function call" $
            (showCode "Compiled: " =<< compileCode Input.callAFunction) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callAFunction
                compiled `shouldBe` expected)
        it "Assignment numeric Literal" $
            (showCode "Compiled: " =<< compileCode Input.assignNumLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignNumLit
                compiled `shouldBe` expected)
        
        it "Assignment binary Operation" $
            (showCode "Compiled: " =<< compileCode Input.assignBinOp) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOp
                compiled `shouldBe` expected)
    
        it "Assignment augmented" $
            (showCode "Compiled: " =<< compileCode Input.assignAugmented) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignAugmented
                compiled `shouldBe` expected)
        
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
        -}
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
        {-
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

        it "Multiassignment no Return" $
            (showCode "Compiled: " =<< compileCode Input.multiAssignment) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multiAssignment
                compiled `shouldBe` expected)

        
        it "Assign many vars, return one" $
            (showCode "Compiled: " =<< compileCode Input.otherVarReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.otherVarReturn
                compiled `shouldBe` expected)
        it "Return a function call" $
            (showCode "Compiled: " =<< compileCode Input.returnFunCall) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.returnFunCall
                compiled `shouldBe` expected)

        it "Simple compose" $
            (showCode "Compiled: " =<< compileCode Input.simpleCompose) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.simpleCompose
                compiled `shouldBe` expected)
        it "Nested function calls" $
            (showCode "Compiled: " =<< compileCode Input.nestedCompose) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.simpleCompose
                compiled `shouldBe` expected) 
                
        
        -}