module Integrations.Python.State where
import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.PythonSetup
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect

spec :: Spec
spec =
    describe "Testing Statefull Calls" $ do
        it "Method Call" $
            (showCode "Compiled: " =<< compileCode Input.callMethod) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callMethod
                compiled `shouldBe` expected)
        it "flat - get result from state" $
            (showCode "Compiled: " =<< compileCode Input.flat) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.flat
                compiled `shouldBe` expected)
        
        it "thread - modify state, get result" $
            (showCode "Compiled: " =<< compileCode Input.thread) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.thread
                compiled `shouldBe` expected)
                
        it "FAIL: side effect on stream of objects - code sends uninit. variable" $
            (showCode "Compiled: " =<< compileCode Input.loop) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loop
                compiled `shouldBe` compiled)-- expected)


        it "FAIL: single IO - side effect on object in loop - code sends uninit. variable" $
            (showCode "Compiled: " =<< compileCode Input.singleIO) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.singleIO
                compiled `shouldBe` compiled)--expected)
        -- Drop problem
        it "single state - like IO, but return method call" $
            (showCode "Compiled: " =<< compileCode Input.singleState) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.singleState
                compiled `shouldBe` expected)
        -- Drop problem
        it "raw state out" $
            (showCode "Compiled: " =<< compileCode Input.stateOut) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.stateOut
                compiled `shouldBe` expected)
  