{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.M3.TestCode.BasicsCode where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

hello_world :: SourceFile Span
hello_world = [sourceFile|
                use funs::hello_world;

                fn test() -> String {
                    hello_world()
                }
                |]

simple_composition :: SourceFile Span
simple_composition = [sourceFile|
                use funs::{f, g};

                fn test() -> String {
                    let x:i32 = f();
                    g(x)
                }
                |]

-- FIXME see issue ohua-lang/ohua-frontend#8
-- enforce Arc construction via type-check
--            (showCode "Compiled: " =<< compileCode  ) `shouldThrow` anyErrorCall
multi_var :: SourceFile Span
multi_var = [sourceFile|
                use funs::*;

                fn test() -> String {
                    let x:i32 = f();
                    let y:i32 = h(x);
                    h2(x,y)
                }
                |]

 -- due to the transformation to state threads, this transforms into:
          -- let x = f();
          -- let x1 = Arc::new();
          -- let (x2,x1') = x1.arc_clone();
          -- let y = h(x1');
          -- h2(x2,y)
          -- where no variable is used more than once!
          -- FIXME(feliix42): At some point we'll have to adjust the data types of the functions used here in `Util.hs` because they currently do not make sense as they don't account for the `Arc`

multi_var_read_only :: SourceFile Span
multi_var_read_only =  [sourceFile|
                use funs::*;

                fn test() -> String {
                    let x:i32 = f();
                    let x1:Arc<i32> = std::sync::Arc::new(x);
                    let x2:Arc<i32> = x1.clone();
                    let y:i32 = h(x1);
                    h2(x2,y)
                }
                |]  

-- due to the transformation to state threads, this transforms into:
          -- let x = f();
          -- let (x1,x') = x.clone();
          -- let y = h(x');
          -- h2(x1,y)
          -- where no variable is used more than once!
multi_var_expl_clone :: SourceFile Span
multi_var_expl_clone = [sourceFile|
                use funs::*;

                fn test() -> String {
                    let x:String = f();
                    let x1:String = x.clone();
                    let y:String = h(x);
                    h2(x1,y)
                }
                |]

env_vars :: SourceFile Span
env_vars =  [sourceFile|
                use funs;

                fn test(i: i32) -> String {
                    let x:i32 = funs::h(i);
                    funs::g(x)
                }
                |]

-- Question: I wonder why I didn't notice until now but ...Why are tasks doubled in 
    -- algo() and test()?
algo_loading :: SourceFile Span
algo_loading = [sourceFile|
                use funs::*;

                fn algo(i: i32) -> String {
                    let x:i32 = h(i);
                    g(x)
                }

                fn test() -> String {
                    algo(4)
                }
                |]

algo_loading_env :: SourceFile Span
algo_loading_env =  [sourceFile|
                use funs;

                fn algo(i: i32) -> String {
                    let x:i32 = funs::h(i);
                    funs::g(x)
                }

                fn test() -> String {
                    algo(4)
                }
                |]


-- Tuple Tests
tuple_from_unit_fun :: SourceFile Span
tuple_from_unit_fun = [sourceFile|
                use funs::*;

                fn test() -> i32 {
                    let (x0,y0):(i32,String) = f_tup();
                    let x1:i32 = f0(x0);
                    let y1:String = f1(y0);
                    h2(x1,y1)
                }
                |]

tuple_from_param :: SourceFile Span
tuple_from_param = [sourceFile|
                use funs::*;

                fn test() -> i32 {
                    let (x0,y0):(i32,String) = f_tup(i32);
                    let x1:i32 = f0(x0);
                    let y1:String = f1(y0);
                    h2(x1,y1)
                }
                |]

smap_for_unbound :: SourceFile Span
smap_for_unbound = [sourceFile|
                use funs::*;
                
                fn test(i:i32) -> () {
                    let s:State = S::new_state();
                    for e in range_from(i) {
                        let r:i32 = h(e);
                        s.gs(r);
                    }
                }
                
                |]

smap_for_bound :: SourceFile Span
smap_for_bound = [sourceFile|
                use funs::*;

                fn test() -> S {
                    let s:State = S::new_state();
                    let stream:Iter<i32> = iter_i32();
                    for e in stream {
                        let r:i32 = h(e);
                        s.gs(r);
                    }
                    s
                }
|]

smap_while:: SourceFile Span
smap_while = [sourceFile|
                use funs::*;

                fn test() -> I32 {
                    //let state:State = S::new_state();
                    let mut i:i32 = I32::new(1);
                    while i.islowerthan23() {
                        //let e:i32
                        //state.gs(i);
                        // i = i + 1 is actually a stateful function acting on a named memory slot
                        i.add(1); 
                    }
                    i
                }
|]      

if_recursion:: SourceFile Span
if_recursion = [sourceFile|
                use funs::*;

                fn algo_rec(i:i32, state:S) -> S{
                    if islowerthan23(i) {
                        state.gs(i);
                        algo_rec(add(i,1), state)
                    } else {
                        state
                    }
                }

                fn test(i:i32) -> S {
                    let mut state:State = S::new_state();
                    algo_rec(i, state) 
                }
|]

if_recursion_only_call_in_branch:: SourceFile Span
if_recursion_only_call_in_branch = [sourceFile|
                use funs::*;

                fn algo_rec(i:i32, state:S) -> S{
                    state.gs(i);
                    let i_new = add(i, 1);
                    if islowerthan23(i) {
                        algo_rec(i_new, state)
                    } else {
                        state
                    }
                }

                fn test(i:i32) -> S {
                    let mut state = S::new_state();
                    algo_rec(i, state) 
                }
|]

if_binop:: SourceFile Span
if_binop = [sourceFile|
                use funs::*;

                fn test(i:i32) -> i32{
                    if i < 13 {
                        i
                    } else {
                        i+1
                    }
                }
|]

