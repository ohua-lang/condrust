module Integrations.Rust.RustM3.TestCases where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.RustM3.RustM3Setup
import Integrations.Rust.RustM3.RustTestCode.BasicsCode as Input
import Integrations.Rust.RustM3.RustTestCode.BasicsOutput as Expect

spec :: Spec
spec =
    describe "Basic Constructs" $ do
        it "hello world" $ 
            (showCode "Compiled: " =<< compileCode Input.hello_world) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.hello_world
                compiled `shouldBe` expected)
        it "Smap on unbound range" $ 
            (showCode "Compiled: " =<< compileCode Input.smap_unbound) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.hello_world
                compiled `shouldBe` expected)

        it "simple composition" $ 
            (showCode "Compiled: " =<< compileCode Input.simple_composition) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.simple_composition
                compiled `shouldBe` expected)
        it "function with two calculated params" $ 
            (showCode "Compiled: " =<< compileCode Input.multi_var) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multi_var
                compiled `shouldBe` expected)
        it "Arc and clone local var" $ 
            (showCode "Compiled: " =<< compileCode Input.multi_var_read_only) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multi_var_read_only
                compiled `shouldBe` expected)
        it "Clone local var" $ 
            (showCode "Compiled: " =<< compileCode Input.multi_var_expl_clone) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multi_var_expl_clone
                compiled `shouldBe` expected)
        it "Use imported function" $ 
            (showCode "Compiled: " =<< compileCode Input.env_vars) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.env_vars
                compiled `shouldBe` expected)
        it "Use algo in other algo" $ 
            (showCode "Compiled: " =<< compileCode Input.algo_loading) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.algo_loading
                compiled `shouldBe` expected)
        it "Use algo with imported function in other algo" $ 
            (showCode "Compiled: " =<< compileCode Input.algo_loading_env) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.algo_loading_env
                compiled `shouldBe` expected)
        it "Tuple destruct from unit fun" $ 
            (showCode "Compiled: " =<< compileCode Input.tuple_from_unit_fun) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tuple_from_unit_fun
                compiled `shouldBe` expected)
        it "Tuple destruct from param fun" $ 
            (showCode "Compiled: " =<< compileCode Input.tuple_from_param) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tuple_from_param
                compiled `shouldBe` expected)     
