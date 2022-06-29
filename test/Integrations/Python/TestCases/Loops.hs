module Integrations.Python.TestCases.Loops where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.PythonSetup
import qualified Integrations.Python.PythonCodeSamples.TestDataInput as Input
import qualified Integrations.Python.PythonCodeSamples.TestDataOutput as Expect


spec :: Spec
spec =
    describe "Loop Statements" $ do
        it "ForLoop over iterator" $
            (showCode "Compiled: " =<< compileCode Input.loopIterator) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterator
                compiled `shouldBe` expected)

        it "ForLoop over iterator an object" $
            (showCode "Compiled: " =<< compileCode Input.loopIterObj) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterObj
                compiled `shouldBe` expected)
        
        it "For loop 3 pipelined Tasks" $
            (showCode "Compiled: " =<< compileCode Input.loop3) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loop3
                compiled `shouldBe` expected)

        it "ERROR: Nested For-Loop, updating State"$
            compileCode Input.nested `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCode Input.nested) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterObj
                compiled `shouldBe` expected)
            -}

        it "FAIL: ForLoop with tuple pattern - Tuple not send to looping task " $
            (showCode "Compiled: " =<< compileCode Input.loopTuplePattern) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopTuplePattern
                compiled `shouldBe` compiled)--expected)

        it "ERROR: ForLoop with comma separated vars" $
            compileCode Input.loopCommaPattern `shouldThrow` anyException
        {-
            (showCode "Compiled: " =<< compileCode Input.loopCommaPattern) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopCommaPattern
                compiled `shouldBe` expected)
        -}
      
        it "ERROR: While loop" $
            compileCodeWithRec Input.whileLoop `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCodeWithRec Input.whileLoop) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.whileLoop
                compiled `shouldBe` expected)
            -}

