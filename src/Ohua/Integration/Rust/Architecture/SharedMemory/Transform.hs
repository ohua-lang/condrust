module Ohua.Integration.Rust.Architecture.SharedMemory.Transform where

import Ohua.Prelude ((.))
import Ohua.Backend.Types

import Ohua.Integration.Architecture
import Ohua.Integration.Transform.DataPar hiding (amorphous)
import Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar
import Ohua.Integration.Rust.Backend.Passes (propagateMut)

instance TypePropagation (Architectures 'SharedMemory)

instance Transform (Architectures 'SharedMemory) where
  transformTaskExprAndChans = lowerTaskPar
  transformTask _ arch = propagateMut . amorphous . spawnWork arch
