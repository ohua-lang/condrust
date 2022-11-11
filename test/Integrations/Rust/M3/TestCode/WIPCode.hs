{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.TestCode.WIPCode where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

generic_types :: SourceFile Span
generic_types = [sourceFile|
                fn test<T>(i:T) -> Result<i32> {
                   let some:i32 = do_stuff(i);
                   Ok(some)
                }
|]

trait_object_types :: SourceFile Span
trait_object_types = [sourceFile|
                fn test(i:Box<dyn Sometrait>) -> Result<i32> {
                   let some: i32 = do_stuff(i);
                   Ok(3)
                }
|]

reference_types :: SourceFile Span
reference_types = [sourceFile|
                fn test(i:&String) -> Result<i32> {
                   let some: i32 = do_stuff(i);
                   Ok(some)
                }
|]

reference_mut_types :: SourceFile Span
reference_mut_types = [sourceFile|
                fn test(i:&mut String) -> Result<i32> {
                   let some: i32 = do_stuff(i);
                   Ok(some)
                }
|]

mut_types :: SourceFile Span
mut_types = [sourceFile|
                fn test(mut i:String) -> Result<i32> {
                   let some: i32 = do_stuff(i);
                   Ok(some)
                }
|]

raw_pointer_types :: SourceFile Span
raw_pointer_types  = [sourceFile|
                fn test(i: *const SomeType) -> Result<i32> {
                   let some: i32 = do_stuff(i);
                   Ok(some)
                }
|]

raw_pointer_types_mut :: SourceFile Span
raw_pointer_types_mut  = [sourceFile|
                fn test(i: *mut SomeType) -> Result<i32> {
                   let some: i32 = do_stuff(i);
                   Ok(some)
                }
|]

function_pointer_types :: SourceFile Span
function_pointer_types  = [sourceFile|
                fn test(do_stuff: fn(i32, i32) -> i32) -> Result<i32> {
                   let some: i32 = do_stuff();
                   Ok(some)
                }
|]

function_pointer_2_types :: SourceFile Span
function_pointer_2_types  = [sourceFile|
                fn test(do_stuff: fn(i32, i32) -> i32) -> Result<i32> {
                   let some: i32 = execute(do_stuff);
                   Ok(some)
                }
|]

or_test = [sourceFile|
    fn test() -> bool{
        let a : bool = f();
        let b: bool = g();
        if a || b {true}
        else {false}
    }
|]


mini = [sourceFile|
                fn test() -> () {
                    let RECOGNIZEABLENAME = ();
                }
|]

if_type_propagation = [sourceFile|
                fn test(i: i32) -> SomeWildeType {
                    let a: i32 = f0(i);
                    let b: i32 = f1(i);
                    let c: i32 = f2(i);
                    let d: i32 = 
                    if a {
                        g0(b)
                    } else {
                        g1(c)
                    };
                    h(d)
                }
|]

loop_type_propagation :: SourceFile Span
loop_type_propagation = [sourceFile|
                fn test() -> S {
                    let s:S = S::new_state();
                    let stream: Iterator<S> = iter_i32();
                    for e in stream {
                        let e1: S = e; 
                        let r: i32 = h(e1);
                        s.gs(r);
                    }
                    s
                }
|]

smap_for_unbound :: SourceFile Span
smap_for_unbound = [sourceFile|
                use funs::*;
                
                fn test(i:i32) -> () {
                    let s:State = S::new_state();
                    for e in range_from(i) {
                        let e1:i32 = e;
                        let r:i32 = h(e1);
                        s.some_method(r);
                    }
                    s
                }
                
                |]

recursion_type_propagation :: SourceFile Span
recursion_type_propagation = [sourceFile|
                fn rec(one:i32, two:i32) -> i32 {
                    let i: i32 = h(one);
                    let i1: i32 = i.clone();
                    let k: i32 = h2(two, i);
                    let k1: i32 = k.clone();
                    if check(k) {
                        rec(k1,i1)
                    } else {
                        k1
                    }
                }

                fn test() -> i32 {
                    rec(2,4)
                }
|]

check_ssa :: SourceFile Span
check_ssa = [sourceFile|

                fn test(y:String)-> i32{
                    let a:i32 = f(y);
                    let a:i32 = f2(a);
                    let a:i32 = f3(a);
                    let a:i32 = f4(a);
                    let a:i32 = f5(a);
                    f6(a)
                }
|]

check_return_types :: SourceFile Span
check_return_types = [sourceFile|

                fn test(y:String) -> OutermostType {
                    let a:FIRST_TYPE = f(y);
                    let b:SECOND_TYPE = f2(a);
                    let c:BRANCH_TYPE = 
                        if BOOLFUN() {
                            g()
                        } else {
                            h()
                        };
                    last_fun(c)
                }
|]

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
