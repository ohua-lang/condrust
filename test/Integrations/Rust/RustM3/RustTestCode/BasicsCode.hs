{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.RustM3.RustTestCode.BasicsCode where

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
                    let x = f();
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
                    let x = f();
                    let y = h(x);
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
                    let x1 = std::sync::Arc::new(x);
                    let x2 = x1.clone();
                    let y = h(x1);
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
                    let x = f();
                    let x1 = x.clone();
                    let y = h(x);
                    h2(x1,y)
                }
                |]

env_vars :: SourceFile Span
env_vars =  [sourceFile|
                use funs;

                fn test(i: i32) -> String {
                    let x = funs::h(i);
                    funs::g(x)
                }
                |]

-- Question: I wonder why I didn't notice until now but ...Why are tasks doubled in 
    -- algo() and test()?
algo_loading :: SourceFile Span
algo_loading = [sourceFile|
                use funs::*;

                fn algo(i: i32) -> String {
                    let x = h(i);
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
                    let x = funs::h(i);
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
                    let (x0,y0) = f_tup();
                    let x1 = f0(x0);
                    let y1 = f1(y0);
                    h2(x1,y1)
                }
                |]

tuple_from_param :: SourceFile Span
tuple_from_param = [sourceFile|
                use funs::*;

                fn test(i:i32) -> i32 {
                    let (x0,y0) = fi_tup(i);
                    let x1 = f0(x0);
                    let y1 = f1(y0);
                    h2(x1,y1)
                }
                |]

smap_for_unbound :: SourceFile Span
smap_for_unbound = [sourceFile|
                use funs::*;
                
                fn test(i:i32) -> () {
                    let s = S::new_state();
                    for e in range_from(i) {
                        let r = h(e);
                        s.gs(r);
                    }
                }
                
                |]

smap_for_bound :: SourceFile Span
smap_for_bound = [sourceFile|
                use funs::*;

                fn test() -> S {
                    let s = S::new_state();
                    let stream = iter_i32();
                    for e in stream {
                        let r = h(e);
                        s.gs(r);
                    }
                    s
                }
|]

smap_while:: SourceFile Span
smap_while = [sourceFile|
                use funs::*;

                fn test() -> S {
                    let state = S::new_state();
                    let mut i = 1;
                    while islowerthan23(i) {
                        state.gs(i);
                        i = add(i, 1);
                    }
                    s
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
                    let mut state = S::new_state();
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

