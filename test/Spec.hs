import Prelude

import Test.Hspec

import qualified LoweringSpec as Lowering
import qualified ConfigSpec as Config
import qualified Integrations.Rust.Spec as Rust
import qualified Integrations.Python.Spec as Python


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LoweringSpec" Lowering.spec
  describe "ConfigSpec" Config.spec
  describe "Rust Integration" Rust.spec
  describe "Python Integration" Python.spec
