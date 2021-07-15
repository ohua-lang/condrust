module Ohua.Integration.Rust.Architecture.SharedMemory.Transform where

import Ohua.Integration.Architecture
import Ohua.Integration.Transform.DataPar
import Ohua.Integration.Rust.Architecture.SharedMemory.Transform.DataPar

instance Transform (Architectures 'SharedMemory) where
  transformTaskExpr = lowerTaskPar
