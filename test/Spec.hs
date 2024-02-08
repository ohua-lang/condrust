import Prelude

import Test.Hspec

import qualified Integrations.Rust.TypeHandlingSpec as RustTyExtract


main :: IO ()
main = hspec spec

spec :: Spec
spec = -- do
  describe "Rust Type Extraction Spec" RustTyExtract.spec
