module Integrations.Rust.Spec
    ( spec
    ) where

import Ohua.Prelude

import Test.Hspec

import qualified Integrations.Rust.Basic as Basic
import qualified Integrations.Rust.If as If
import qualified Integrations.Rust.State as State
import qualified Integrations.Rust.SMap as SMap
import qualified Integrations.Rust.TailRec as TailRec
import qualified Integrations.Rust.Benchmark as Benchmark
import qualified Integrations.Rust.Control as Control


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
