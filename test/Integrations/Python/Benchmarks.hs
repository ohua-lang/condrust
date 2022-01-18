module Integrations.Python.Benchmarks where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.BenchInput as Input
import qualified Integrations.Python.BenchOutput as Expect


spec :: Spec
spec =
    describe "Testing Benchmmark Comp." $ do 
        {-
        it "Black Scholes" $
            (showCode "Compiled: " =<< compileCode Input.blackScholes) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.blackScholes
                compiled `shouldBe` expected)
        -}
        it "Natural Parallelism 4 tasks" $
            (showCode "Compiled: " =<< compileCode Input.natPar) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.natPar
                compiled `shouldBe` expected)
        it "Natural Parallelism 8 tasks" $
            (showCode "Compiled: " =<< compileCode Input.natPar7) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.natPar7
                compiled `shouldBe` expected)
        it "Natural Parallelism 16 == #CPU tasks" $
            (showCode "Compiled: " =<< compileCode Input.natPar15) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.natPar15
                compiled `shouldBe` expected)
        it "Natural Parallelism 32 == 2*#CPU tasks" $
            (showCode "Compiled: " =<< compileCode Input.natPar31) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.natPar31
                compiled `shouldBe` expected)
        {-  Can't do this without 'input-reuse'
        it "For loop 3 parallel Tasks" $
            (showCode "Compiled: " =<< compileCode Input.loop3') >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loop3
                compiled `shouldBe` expected)
        -}

        it "For loop 3 pipelined Tasks" $
            (showCode "Compiled: " =<< compileCode Input.loop3) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loop3
                compiled `shouldBe` expected)