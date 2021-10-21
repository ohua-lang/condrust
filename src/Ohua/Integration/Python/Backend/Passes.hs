module Ohua.Integration.Python.Backend.Passes where

import qualified Data.HashSet as HS
import qualified Ohua.Core.DFLang.Passes.State as StateDFL
import Ohua.Integration.Transform.DataPar (dataPar)
import Ohua.Prelude


-- TODO configuration flag
enableDataPar =False

passes = StateDFL.load $ if enableDataPar then dataPar else def