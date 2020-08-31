module Ohua.Integration.Rust.Types where

import System.FilePath
import Language.Rust.Syntax hiding (Rust)
import Language.Rust.Parser ( Span )

newtype Rust = Rust ()
newtype Module = Module (FilePath, SourceFile Span)
