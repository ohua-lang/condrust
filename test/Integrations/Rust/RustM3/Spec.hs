module Integrations.Rust.RustM3.Spec
    ( spec
    ) where

import Ohua.Prelude

import Test.Hspec

import qualified Integrations.Rust.RustM3.TestCases as Cases


spec :: Spec
spec =
    Cases.spec 