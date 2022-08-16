module Integrations.Rust.RustM3.Spec
    ( spec
    ) where

import Ohua.Prelude

import Test.Hspec

import qualified Integrations.Rust.RustM3.WIPCases as WIPCases
import qualified Integrations.Rust.RustM3.TestCases as BasicCases
import qualified Integrations.Rust.RustM3.KVStoreCase as KVCase


spec :: Spec
spec =
    --WIPCases.spec
    BasicCases.spec -- >>
    -- KVCase.spec