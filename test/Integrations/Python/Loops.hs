module Integrations.Python.Loops where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "Loop Statements" $ do
        it "ForLoop over iterator" $
            (showCode "Compiled: " =<< compileCode Input.loopIterator) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterator
                compiled `shouldBe` expected)

        it "ForLoop over iterator on object" $
            (showCode "Compiled: " =<< compileCode Input.loopIterObj) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterObj
                compiled `shouldBe` expected)
     
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
        it "Assign list comprehension" $
            (showCode "Compiled: " =<< compileCode Input.assignListCompr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignListCompr
                compiled `shouldBe` expected)

        it "list comprehension equal to for " $
            (showCode "Compiled: " =<< compileCode Input.justListCompr) >>=
            (\compiled -> do
                expected <- showCode "Expected:"  =<< compileCode Input.justListComprComp
                compiled `shouldBe` expected)


