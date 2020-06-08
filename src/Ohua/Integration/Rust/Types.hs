module Ohua.Integration.Rust.Types where

import System.FilePath
import Language.Rust.Syntax hiding (Rust)
import Language.Rust.Parser ( Span )

data RustLang where
    Rust :: RustLang
    Module :: (FilePath, SourceFile Span) -> RustLang
