module Integrations.Rust.RustSharedMemory.Spec
    ( spec
    ) where

import Ohua.Prelude

import Test.Hspec

import qualified Integrations.Rust.RustSharedMemory.Basic as Basic
import qualified Integrations.Rust.RustSharedMemory.If as If
import qualified Integrations.Rust.RustSharedMemory.State as State
import qualified Integrations.Rust.RustSharedMemory.SMap as SMap
import qualified Integrations.Rust.RustSharedMemory.TailRec as TailRec
import qualified Integrations.Rust.RustSharedMemory.Benchmark as Benchmark
import qualified Integrations.Rust.RustSharedMemory.Control as Control
import qualified Integrations.Rust.RustSharedMemory.Scoping as Scoping


spec :: Spec
spec =
    Basic.spec >>
    If.spec >>
    State.spec >>
    SMap.spec >>
    TailRec.spec >>
    -- ohua-lang/ohuac#32
    -- Benchmark.spec >>
    Control.spec >>
    Scoping.spec