module Integrations.Python.Spec
    ( spec
    ) where

import Ohua.Prelude

import Test.Hspec

import qualified Integrations.Python.Basic as Basic
import qualified Integrations.Python.IfElse as IfElse
import qualified Integrations.Python.Loops as Loops
import qualified Integrations.Python.State as State
import qualified Integrations.Python.TailRec as TailRec
import qualified Integrations.Python.WIP as WIP
import qualified Integrations.Python.Benchmarks as BMs


spec :: Spec
spec = 
    -- Basic.spec -- >> 
    IfElse.spec -- >>
    -- Loops.spec  -- >>
    -- TailRec.spec
    -- State.spec -- >>
    -- BMs.spec -- >>
    -- WIP.spec

