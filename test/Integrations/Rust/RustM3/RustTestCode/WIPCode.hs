{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.RustM3.RustTestCode.WIPCode where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

return_literal :: SourceFile Span
return_literal = [sourceFile|
                use funs::hello_world;

                fn test() -> String {
                    let x = hello_world();
                    "literal"
                }
                |]

assign_literal :: SourceFile Span
assign_literal = [sourceFile|
                use funs::*;

                fn test() -> String {
                    let x = 3;
                    f(x)
                }
                |]