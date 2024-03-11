module Ohua.Integration.Python.Backend.Passes where

import Ohua.Commons.Prelude
import Ohua.Integration.Options (Options)
import Ohua.Core.Compile.Configuration (CustomPasses)
import qualified Ohua.Core.DFLang.Passes.State as StateDFL
import Ohua.Integration.Transform.DataPar (dataPar)


passes :: Options -> (ty -> ty) -> CustomPasses embExpr annot ty
passes opts = StateDFL.load . dataPar opts
