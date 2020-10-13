module Ohua.Integration.Architecture where

data Arch = SharedMemory

data Architectures :: Arch -> * where
    SSharedMemory :: Architectures 'SharedMemory
