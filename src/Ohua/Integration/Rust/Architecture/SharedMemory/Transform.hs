module Ohua.Integration.Rust.Architecture.SharedMemory.Transform where

import Ohua.Commons.Prelude ((.))
import Ohua.Backend.Types

import Ohua.Integration.Architecture
import Ohua.Integration.Transform.DataPar hiding (amorphous)
import Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar
import Ohua.Integration.Rust.Backend.Passes (propagateMut)
import Ohua.Integration.Rust.Architecture.SharedMemory ()

instance Transform (Architectures 'SharedMemory) where
  transformTaskExpr = lowerTaskPar
  transformTask _ arch = propagateMut . amorphous . spawnWork arch
