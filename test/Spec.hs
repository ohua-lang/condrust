import Prelude

import Test.Hspec

import qualified LoweringSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "LoweringSpec" LoweringSpec.spec
