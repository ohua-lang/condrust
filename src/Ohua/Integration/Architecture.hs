module Ohua.Integration.Architecture where

data Arch = SharedMemory | M3

data Architectures :: Arch -> * where
    SSharedMemory :: Architectures 'SharedMemory
    SM3 :: Architectures 'M3


