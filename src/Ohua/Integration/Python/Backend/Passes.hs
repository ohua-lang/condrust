module Ohua.Integration.Python.Backend.Passes where

import qualified Data.HashSet as HS
import qualified Ohua.Core.DFLang.Passes.State as StateDFL
import Ohua.Integration.Transform.DataPar (dataPar)
import Ohua.Prelude



passes = StateDFL.load . dataPar