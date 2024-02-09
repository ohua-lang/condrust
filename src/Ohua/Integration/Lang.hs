module Ohua.Integration.Lang where

import Ohua.Commons.Prelude (Show)
import GHC.Enum ( Bounded, Enum )

data Lang = Rust | Python deriving (Show, Enum, Bounded)

data Language :: Lang -> * where
    SRust :: Language 'Rust
    SPython :: Language 'Python
