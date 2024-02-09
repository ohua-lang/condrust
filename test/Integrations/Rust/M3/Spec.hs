module Integrations.Rust.M3.Spec
    ( spec
    ) where

import Ohua.Commons.Prelude

import Test.Hspec

import qualified Integrations.Rust.M3.WIPCases as WIPCases
import qualified Integrations.Rust.M3.Basics as Basics
import qualified Integrations.Rust.M3.KVStoreCase as KVCase


spec :: Spec
spec =
    Basics.spec -- >>
    -- WIPCases.spec
    -- KVCase.spec
