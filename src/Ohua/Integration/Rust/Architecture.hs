module Ohua.Integration.Rust.Architecture where

import Language.Rust.Syntax hiding (Rust)

import Ohua.Backend.Types
import Ohua.Integration.Rust.Types as RT
import qualified Ohua.Integration.Rust.Architecture.SharedMemory as SM


data Architectures = SharedMemory 

instance Architecture Architectures where 
    type Integ Architectures = RT.Module
    type Task Architectures = Expr ()
    type Chan Architectures = Stmt ()

    build SharedMemory = SM.build
    serialize SharedMemory = SM.serialize
