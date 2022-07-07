module Integrations.Rust.RustSTM.Spec
    ( spec
    ) where

import Ohua.Prelude

import Test.Hspec

import qualified Integrations.Rust.RustSTM.Basic as Basic
import qualified Integrations.Rust.RustSTM.If as If
import qualified Integrations.Rust.RustSTM.State as State
import qualified Integrations.Rust.RustSTM.SMap as SMap
import qualified Integrations.Rust.RustSTM.TailRec as TailRec
import qualified Integrations.Rust.RustSTM.Benchmark as Benchmark
import qualified Integrations.Rust.RustSTM.Control as Control


spec :: Spec
spec =
    Basic.spec >>
    If.spec >>
    State.spec >>
    SMap.spec >>
    TailRec.spec >>
    -- ohua-lang/ohuac#32
    -- Benchmark.spec >>
    Control.spec
