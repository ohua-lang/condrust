{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.RustM3.RustTestCode.WIPCode where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

return_literal :: SourceFile Span
return_literal = [sourceFile|
                use funs::hello_world;

                fn test() -> String {
                    let x:String = hello_world();
                    "literal"
                }
                |]

assign_literal :: SourceFile Span
assign_literal = [sourceFile|
                use funs::*;
                use std;

                fn test() -> String {
                    let x:i32 = f();
                    let y:i32 = std::std_fun(x);
                    f(y)
                }
                |]

binary_operations :: SourceFile Span
binary_operations = [sourceFile|
                fn test() -> i32 {
                    let x:i32 = 0;
                    let y:i32 = 42;
                    let z:i32 = x + y;
                    let z1:i23 = z * 2;
                    z1 
                }
|]