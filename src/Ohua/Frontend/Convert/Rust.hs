module Ohua.Frontend.Convert.Rust where

import Ohua.Prelude

import Ohua.Frontend.Convert
import Ohua.Frontend.Lang as FrLang
import Language.Rust.Syntax as Rust
import Language.Rust.Data.Ident


instance ConvertFrom (Rust.Expr a) where 
    convertFrom = undefined -- TODO