{-# LANGUAGE QuasiQuotes #-}

module Integrations.Rust.RustM3.KVAppCode where
    
import Language.Rust.Quote

kv_application = [sourceFile|
                use funs::*;

                fn test() -> String {
                    let x:i32 = f();
                    let x1 = std::sync::Arc::new(x);
                    let x2 = x1.clone();
                    let y = h(x1);
                    h2(x2,y)
                }
                |]

interface_code = 
    [sourceFile|
                use funs::*;

                fn test() -> String {
                    let x:i32 = f();
                    let x1 = std::sync::Arc::new(x);
                    let x2 = x1.clone();
                    let y = h(x1);
                    h2(x2,y)
                }
                |]