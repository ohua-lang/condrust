module Integrations.Rust.RustM3.TestCases where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.RustM3.RustM3Setup
import Integrations.Rust.RustM3.RustTestCode.BasicsCode as Input
import Integrations.Rust.RustM3.RustTestCode.BasicsOutput as Expect

spec :: Spec
spec =
    describe "Basic Constructs" $ do
        it "hello world" $ 
            (showCode "Compiled: " =<< compileCodeWithDebug Input.hello_world) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.hello_world
                compiled `shouldBe` expected)
 
        it "simple composition" $ 
            (showCode "Compiled: " =<< compileCodeWithDebug Input.simple_composition) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.simple_composition
                compiled `shouldBe` expected)
{-
        it "While Loop" $ 
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug Input.smap_while) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.hello_world
                compiled `shouldBe` expected)
       
        it "While Loop as If-Recursion" $ 
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug Input.if_recursion) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.hello_world
                compiled `shouldBe` expected)

        it "FAIL: While Loop as If-Recursion only recursive call in branches" $ 
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug Input.if_recursion_only_call_in_branch) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.if_recursion_only_call_in_branch
                compiled `shouldBe` expected)


        it "if condition with binop" $ 
            (showCode "Compiled: " =<< compileCodeWithDebug Input.if_binop) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.simple_composition
                compiled `shouldBe` expected)

        it "Smap/For-Loop bound range" $
            (showCode "Compiled: " =<< compileCode Input.smap_for_bound) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.hello_world
                compiled `shouldBe` expected)
   
        it "Smap/For-Loop on unbound range" $ 
            (showCode "Compiled: " =<< compileCode Input.smap_for_unbound) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.hello_world
                compiled `shouldBe` expected)
       -}         
{-
        
        it "function with two calculated params" $ 
            (showCode "Compiled: " =<< compileCodeWithDebug Input.multi_var) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multi_var
                compiled `shouldBe` expected)
        it "Arc and clone local var" $ 
            (showCode "Compiled: " =<< compileCodeWithDebug Input.multi_var_read_only) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multi_var_read_only
                compiled `shouldBe` expected)
        it "Clone local var" $ 
            (showCode "Compiled: " =<< compileCodeWithDebug Input.multi_var_expl_clone) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multi_var_expl_clone
                compiled `shouldBe` expected)
        it "Use imported function" $ 
            (showCode "Compiled: " =<< compileCodeWithDebug Input.env_vars) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.env_vars
                compiled `shouldBe` expected)
        it "Use algo in other algo" $ 
            (showCode "Compiled: " =<< compileCodeWithDebug Input.algo_loading) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.algo_loading
                compiled `shouldBe` expected)
        it "Use algo with imported function in other algo" $ 
            (showCode "Compiled: " =<< compileCodeWithDebug Input.algo_loading_env) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.algo_loading_env
                compiled `shouldBe` expected)
        it "Tuple destruct from unit fun" $ 
            (showCode "Compiled: " =<< compileCodeWithDebug Input.tuple_from_unit_fun) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tuple_from_unit_fun
                compiled `shouldBe` expected)
        it "Tuple destruct from param fun" $ 
            (showCode "Compiled: " =<< compileCodeWithDebug Input.tuple_from_param) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tuple_from_param
                compiled `shouldBe` expected)     
-}
