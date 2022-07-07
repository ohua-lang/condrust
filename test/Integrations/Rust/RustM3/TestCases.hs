module Integrations.Rust.RustM3.TestCases where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integration.Rust.RustM3.KVAppCode

spec :: Spec
spec =
    describe "KVApplication should compile" $ do
        it "Placeholder test" $
            1 `shouldBe` 1