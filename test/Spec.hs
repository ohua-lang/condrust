import Prelude

import Test.Hspec

import qualified LoweringSpec
import qualified ConfigSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LoweringSpec" LoweringSpec.spec
  describe "ConfigSpec" ConfigSpec.spec
