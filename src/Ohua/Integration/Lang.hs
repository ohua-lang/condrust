module Ohua.Integration.Lang where

import Ohua.Commons.Prelude (Show)
import qualified Data.Kind as DK
import GHC.Enum ( Bounded, Enum )

data Lang = Rust | Python deriving (Show, Enum, Bounded)

data Language :: Lang -> DK.Type where
    SRust :: Language 'Rust
    SPython :: Language 'Python
