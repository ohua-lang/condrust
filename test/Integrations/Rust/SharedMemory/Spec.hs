module Integrations.Rust.SharedMemory.Spec
    ( spec
    ) where

import Ohua.Commons.Prelude

import Test.Hspec

import qualified Integrations.Rust.SharedMemory.Basic as Basic
import qualified Integrations.Rust.SharedMemory.If as If
import qualified Integrations.Rust.SharedMemory.State as State
import qualified Integrations.Rust.SharedMemory.SMap as SMap
import qualified Integrations.Rust.SharedMemory.TailRec as TailRec
import qualified Integrations.Rust.SharedMemory.Benchmark as Benchmark
import qualified Integrations.Rust.SharedMemory.Control as Control
import qualified Integrations.Rust.SharedMemory.Scoping as Scoping
import qualified Integrations.Rust.SharedMemory.ProgModRestrictions as Restrictions
import qualified Integrations.Rust.SharedMemory.WIP as TestTypextraction

spec :: Spec
spec =
    Basic.spec  >>
    TestTypextraction.spec  >>
    If.spec  >>
    State.spec >>
    SMap.spec  >>
    TailRec.spec  >>   
    -- ohua-lang/ohuac#32
    -- Benchmark.spec >>
    Control.spec -- >>
    --Scoping.spec  >>
    -- Restrictions.spec-}
 