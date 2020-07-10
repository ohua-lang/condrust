import Prelude

import Test.Hspec

import qualified LoweringSpec as Lowering
import qualified ConfigSpec as Config
import qualified Integrations.RustSpec as Rust


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LoweringSpec" Lowering.spec
  describe "ConfigSpec" Config.spec
  describe "Rust Integeration Spec" Rust.spec
