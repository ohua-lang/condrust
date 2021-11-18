module Ohua.Integration.Lang where

import Ohua.Prelude (Show)
import GHC.Enum ( Bounded, Enum )

data Lang = Rust deriving (Show, Enum, Bounded)

data Language :: Lang -> * where
    SRust :: Language 'Rust
