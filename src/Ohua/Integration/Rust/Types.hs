module Ohua.Integration.Rust.Types where

import System.FilePath
import Language.Rust.Syntax hiding (Rust)
import Language.Rust.Parser ( Span )

data Rust = Rust
newtype Module = Module (FilePath, SourceFile Span)
