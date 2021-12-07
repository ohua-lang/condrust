module Ohua.Integration.Python.Backend.Passes where

import Ohua.Prelude
import Ohua.Integration.Config (Options)
import Ohua.Core.Compile.Configuration (CustomPasses)
import qualified Ohua.Core.DFLang.Passes.State as StateDFL
import Ohua.Integration.Transform.DataPar (dataPar)


passes :: Options -> CustomPasses
passes = StateDFL.load . dataPar