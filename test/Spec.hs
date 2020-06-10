import Prelude

import Test.Hspec

import qualified ResolveSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "ResolveSpec" ResolveSpec.spec
