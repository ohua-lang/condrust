module Integrations.Rust.RustM3.TestCases where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.RustM3.RustM3Setup
import Integrations.Rust.RustM3.RustTestCode.KVAppCode as Input
import Integrations.Rust.RustM3.RustTestCode.KVAppCompiled as Expect
import Integrations.Rust.RustM3.RustTestCode.BasicsCode as BasicsInput
import Integrations.Rust.RustM3.RustTestCode.BasicsOutput as BasicsExpect

spec :: Spec
spec =
    describe "Basic Constructs" $ do
        it "hello world" $ 
            (showCode "Compiled: " =<< compileCode BasicsInput.hello_world) >>=
            (\compiled -> do
                expected <- showCode "Expected:" BasicsExpect.hello_world
                compiled `shouldBe` expected)
        it "simple composition" $ 
            (showCode "Compiled: " =<< compileCode BasicsInput.simple_composition) >>=
            (\compiled -> do
                expected <- showCode "Expected:" BasicsExpect.simple_composition
                compiled `shouldBe` expected)
        it "function with two calculated params" $ 
            (showCode "Compiled: " =<< compileCode BasicsInput.multi_var) >>=
            (\compiled -> do
                expected <- showCode "Expected:" BasicsExpect.multi_var
                compiled `shouldBe` expected)
        it "Arc and clone local var" $ 
            (showCode "Compiled: " =<< compileCode BasicsInput.multi_var_read_only) >>=
            (\compiled -> do
                expected <- showCode "Expected:" BasicsExpect.multi_var_read_only
                compiled `shouldBe` expected)
        it "Clone local var" $ 
            (showCode "Compiled: " =<< compileCode BasicsInput.multi_var_expl_clone) >>=
            (\compiled -> do
                expected <- showCode "Expected:" BasicsExpect.multi_var_expl_clone
                compiled `shouldBe` expected)
        it "Use imported function" $ 
            (showCode "Compiled: " =<< compileCode BasicsInput.env_vars) >>=
            (\compiled -> do
                expected <- showCode "Expected:" BasicsExpect.env_vars
                compiled `shouldBe` expected)
        it "Use algo in other algo" $ 
            (showCode "Compiled: " =<< compileCode BasicsInput.algo_loading) >>=
            (\compiled -> do
                expected <- showCode "Expected:" BasicsExpect.algo_loading
                compiled `shouldBe` expected)
        it "Use algo with imported function in other algo" $ 
            (showCode "Compiled: " =<< compileCode BasicsInput.algo_loading_env) >>=
            (\compiled -> do
                expected <- showCode "Expected:" BasicsExpect.algo_loading_env
                compiled `shouldBe` expected)
        it "Tuple destruct from unit fun" $ 
            (showCode "Compiled: " =<< compileCode BasicsInput.tuple_from_unit_fun) >>=
            (\compiled -> do
                expected <- showCode "Expected:" BasicsExpect.tuple_from_unit_fun
                compiled `shouldBe` expected)
        it "Tuple destruct from param fun" $ 
            (showCode "Compiled: " =<< compileCode BasicsInput.tuple_from_param) >>=
            (\compiled -> do
                expected <- showCode "Expected:" BasicsExpect.tuple_from_param
                compiled `shouldBe` expected)     

{-
    describe "KVApplication should compile" $ do
        it "KV-Applicaation, 3 Step Loop" $
            (showCode "Compiled: " =<< compileCode Input.kv_application) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.kv_application
                compiled `shouldBe` expected)-}