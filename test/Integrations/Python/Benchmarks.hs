module Integrations.Python.Benchmarks where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.PythonSetup
import qualified Integrations.Python.BenchInput as Input
import qualified Integrations.Python.BenchOutput as Expect


spec :: Spec
spec =
    describe "Testing Benchmmark Comp." $ do
        it "Black Scholes" $
            (showCode "Compiled: " =<< compileCode Input.blackScholes) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.blackScholes
                compiled `shouldBe` expected)