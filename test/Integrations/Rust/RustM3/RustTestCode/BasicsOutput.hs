{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.RustM3.RustTestCode.BasicsOutput where

import Language.Rust.Quote (sourceFile)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

hello_world :: SourceFile Span
hello_world = [sourceFile|
                use funs::hello_world;
                // to be filled
                |]

simple_composition :: SourceFile Span
simple_composition = [sourceFile|
                use funs::hello_world;
                // to be filled
                |]

multi_var :: SourceFile Span
multi_var = [sourceFile|
                use funs::hello_world;
                // to be filled
                |]

multi_var_read_only :: SourceFile Span
multi_var_read_only =  [sourceFile|
                use funs::hello_world;
                // to be filled
                |]  

multi_var_expl_clone :: SourceFile Span
multi_var_expl_clone = [sourceFile|
                use funs::hello_world;
                // to be filled
                |]

env_vars :: SourceFile Span
env_vars =  [sourceFile|
                use funs::hello_world;
                // to be filled
                |]

algo_loading :: SourceFile Span
algo_loading = [sourceFile|
                use funs::hello_world;
                // to be filled
                |]

algo_loading_env :: SourceFile Span
algo_loading_env =  [sourceFile|
                use funs::hello_world;
                // to be filled
                |]


-- Tuple Tests
tuple_from_unit_fun :: SourceFile Span
tuple_from_unit_fun = [sourceFile|
                use funs::hello_world;
                // to be filled
                |]

tuple_from_param :: SourceFile Span
tuple_from_param = [sourceFile|
                use funs::hello_world;
                // to be filled
                |]

          
