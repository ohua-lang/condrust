module Integrations.Python.TestCases.Benchmarks where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Setup
import qualified Integrations.Python.CodeSamples.BenchInput as Input
import qualified Integrations.Python.CodeSamples.BenchOutput as Expect



spec :: Spec
spec =
    describe "Benchmark Tests" $ do 
        it "" $
           1 `shouldBe` 1