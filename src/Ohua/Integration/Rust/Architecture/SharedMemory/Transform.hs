module Ohua.Integration.Rust.Architecture.SharedMemory.Transform where

import Ohua.Prelude ((.))
import Ohua.Backend.Types

import Ohua.Integration.Architecture
import Ohua.Integration.Transform.DataPar hiding (amorphous)
import Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar

instance Transform (Architectures 'SharedMemory) where
  transformTaskExpr = lowerTaskPar
  transformTask _ _ = amorphous . spawnWork
