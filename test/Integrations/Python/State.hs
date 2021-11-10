module Integrations.Python.State where
import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
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