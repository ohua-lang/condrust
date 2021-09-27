module Integrations.Python.WIP where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "WIP Tests" $ do
        it "placeholder test" $
            1 `shouldBe` 1
        it "Method Call" $
            (showCode "Compiled: " =<< compileCode Input.callMethod) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callMethod
                compiled `shouldBe` expected)