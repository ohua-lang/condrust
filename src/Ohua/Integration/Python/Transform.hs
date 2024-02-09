module Ohua.Integration.Python.Transform where

import Ohua.Commons.Prelude (id)
import Ohua.Backend.Types
import Ohua.Integration.Architecture
import Ohua.Integration.Transform.DataPar
import Ohua.Integration.Python.MultiProcessing()


instance Transform (Architectures 'MultiProcessing) where
  transformTaskExpr = lowerTaskPar
  transformTask _ _ = id
