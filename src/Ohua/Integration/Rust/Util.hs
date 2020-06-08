module Ohua.Integration.Rust.Util where

import Ohua.Prelude
import Language.Rust.Data.Ident

toBinding :: Ident -> Binding
toBinding Ident{name=n} = fromString n
