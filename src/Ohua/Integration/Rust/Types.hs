module Ohua.Integration.Rust.Types where

import Ohua.Prelude

import Language.Rust.Syntax
import Language.Rust.Parser


data Module = Module FilePath (SourceFile Span)
