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

scope:: SourceFile Span
scope = [sourceFile|

                fn test() -> i32 {
                    let x:i32 = f();
                    let s:State = State::new();
                    {
                        let x:String = g();
                        s.do_it(x);
                    }
                    h(x)
                }
|]

scope_for:: SourceFile Span
scope_for = [sourceFile|

                fn test() -> i32 {
                    let x:i32 = f();
                    let s:State = State::new();
                    for i in iter() {
                        let x:String = g();
                        s.do_it(x);
                    }
                    h(x)
                }
|]

scope_branch:: SourceFile Span
scope_branch = [sourceFile|

                fn test() -> i32 {
                    let x:i32 = f();
                    let s:State = State::new();
                    if check() {
                        let x:String = g();
                        s.do_it(x)
                        }
                    h(x)
                }
|]


scope_const :: SourceFile Span
scope_const = [sourceFile|

                const global:i32 = 9;

                fn test() -> i32 {
                    let x:i32 = f(global);
                    let y:String = g(x);
                    h(y)
                }
            |]

scope_static :: SourceFile Span
scope_static = [sourceFile|

                const global:i32 = 9;
                static st_thing:String = String::from(" times hello");

                fn test() -> i32 {
                    let x:i32 = f(global);
                    let y:String = second_algo(x);
                    h(y)
                }

                fn second_algo(arg:i32)-> String {
                    // -- ToDo: Missing language construct. 
                    //          The original call would be : let mut as_string:String = arg.to_string();
                    //          But it seems we can not use arguments as states, which should be possible I think.
                    let mut as_string:String = to_string(arg);
                    as_string.push(st_thing)
                }
            |]

scope_mut :: SourceFile Span
scope_mut = [sourceFile|

                static mut st_thing:String = String::from(" times hello");

                fn test() -> i32 {
                    let x:i32 = f(st_thing);
                    let y:String = second_algo(x);
                    h(y)
                }

            |] 