import Prelude

import Test.Hspec

-- Frontend tests
import qualified Frontend.ResolveSpec


-- Compiler Tests
import qualified LoweringSpec as Lowering
import qualified ConfigSpec as Config
import qualified Integrations.Rust.SharedMemory.Spec as Rust
import qualified Integrations.Rust.M3.Spec as RustM3
import qualified Integrations.Python.Spec as Python


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Frontend: ResolveSpec" Frontend.ResolveSpec.spec
  describe "Rust Integrations: Type Extraction Spec" RustTyExtract.spec
--  describe "LoweringSpec" Lowering.spec
  describe "Compiler: ConfigSpec" Config.spec
  describe "Compiler: Rust Integration SharedMemory" Rust.spec
  describe "Compiler: Python Integration" Python.spec
  describe "Compiler: Rust Integration M3" RustM3.spec

