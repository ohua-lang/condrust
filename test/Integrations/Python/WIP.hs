module Integrations.Python.WIP where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "WIP Tests" $ do
        {-
        it "Two Algos" $
            (showCode "Compiled: " =<< compileCode Input.twoAlgos) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callMethod
                compiled `shouldBe` expected)

        -}
        it "Assign list with elements" $
            (showCode "Compiled: " =<< compileCode Input.assignList) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignList
                compiled `shouldBe` expected) 

        it "Assign dict with elements" $
            (showCode "Compiled: " =<< compileCode Input.assignDict) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignDict
                compiled `shouldBe` expected) 

        
        it "Tuple as argument" $
            (showCode "Compiled: " =<< compileCode Input.tupleArgumentCall) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tupleArgumentCall
                compiled `shouldBe` expected) 

        {-Todo: Error : unitFun must only have one output
        it "Multiassignment comma separated" $
            (showCode "Compiled: " =<< compileCode Input.multiAssignment) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multiAssignment
                compiled `shouldBe` expected)
         -}