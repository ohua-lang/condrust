module Integrations.Python.Loops where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "Loop Statements" $ do
        it "placeholder test" $ 
          1 `shouldBe` 1
        it "ForLoop over iterator" $
            (showCode "Compiled: " =<< compileCode Input.loopIterator) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterator
                compiled `shouldBe` expected)
        it "For loop over list" $
            (showCode "Compiled: " =<< compileCode Input.loopList) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopList
                compiled `shouldBe` expected)
