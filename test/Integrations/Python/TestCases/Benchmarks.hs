module Integrations.Python.TestCases.Benchmarks where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.PythonSetup
import qualified Integrations.Python.PythonCodeSamples.BenchInput as Input
import qualified Integrations.Python.PythonCodeSamples.BenchOutput as Expect



spec :: Spec
spec =
    describe "Benchmark Tests" $ do 
        it "" $
            (showCode "Compiled: " =<< compileCode Input.parallel_composition) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Input.parallel_composition
                compiled `shouldBe` compiled)