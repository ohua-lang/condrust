module Integrations.Python.IfElse where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "IfElse Statements" $ do
        it "placeholder test" $ 
          1 `shouldBe` 1