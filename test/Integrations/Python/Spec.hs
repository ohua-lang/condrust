module Integrations.Python.Spec
    ( spec
    ) where

import Ohua.Prelude

import Test.Hspec

import qualified Integrations.Python.TestCases.Basic as Basic
import qualified Integrations.Python.TestCases.IfElse as IfElse
import qualified Integrations.Python.TestCases.Loops as Loops
import qualified Integrations.Python.TestCases.State as State
import qualified Integrations.Python.TestCases.TailRec as TailRec
import qualified Integrations.Python.TestCases.ErrorCases as ToDos
import qualified Integrations.Python.TestCases.WIP as WIP


spec :: Spec
spec = 
    Basic.spec  >> 
    IfElse.spec  >>
    Loops.spec >>
    TailRec.spec >>
    State.spec  >>
    ToDos.spec  >>
    WIP.spec
