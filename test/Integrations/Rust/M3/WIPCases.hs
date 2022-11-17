module Integrations.Rust.M3.WIPCases where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.M3.Setup
import Integrations.Rust.M3.TestCode.WIPCode as Input
import Integrations.Rust.M3.TestCode.WIPOutput as WIPExpect
import Integrations.Rust.M3.TestCode.BasicsOutput as Expect

spec :: Spec
spec =
    describe "Work in Progress Tests" $ do
        describe "We need to fix type extraction" $ do
            it "Generic types" $
                (showCode "Compiled: " =<< compileCode Input.generic_types) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 
            it "Trait objects" $
                (showCode "Compiled: " =<< compileCode Input.trait_object_types) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 
            it "Reference Types" $
                (showCode "Compiled: " =<< compileCode Input.reference_types) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 
            it "Reference Mut Types" $
                (showCode "Compiled: " =<< compileCode Input.reference_mut_types) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 
            it "Mut Types" $
                (showCode "Compiled: " =<< compileCode Input.mut_types) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 
            it "Raw Pointers" $
                (showCode "Compiled: " =<< compileCode Input.raw_pointer_types) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 
            it "Mutable raw pointers" $
                (showCode "Compiled: " =<< compileCode Input.raw_pointer_types_mut) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 
            it "Funnction Pointers" $
                (showCode "Compiled: " =<< compileCode Input.function_pointer_types) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 
            it "Function Pointers used as var" $
                (showCode "Compiled: " =<< compileCode Input.function_pointer_2_types) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 
            
    {-            
        describe "We need to fix type propagation" $ do
            it "test type propagation - if" $
                (showCode "Compiled: " =<< compileCodeWithDebug Input.if_type_propagation) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 
            it "test type propagation - if" $
                (showCode "Compiled: " =<< compileCodeWithDebug Input.if_type_propagation) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 

            it "test type propagation - loop " $
                (showCode "Compiled: " =<< compileCodeWithDebug Input.loop_type_propagation) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 

            it "test type propagation - loop 2 " $
                (showCode "Compiled: " =<< compileCodeWithDebug Input.smap_for_unbound) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 
                    
            it "test type propagation - recursion" $
                (showCode "Compiled: " =<< compileCodeWithRecWithDebug Input.recursion_type_propagation) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 

            it "test ssa renaming " $
                (showCode "Compiled: " =<< compileCodeWithDebug Input.check_ssa) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 

            it "return type scopes " $
                (showCode "Compiled: " =<< compileCodeWithDebug Input.check_return_types) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) -}
            
    {-    
            it "poll1 - as recursion" $
                (showCode "Compiled: " =<< compileCode Input.poll1_as_rec) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 

            it "use parameter as state" $
                (showCode "Compiled: " =<< compileCode Input.use_state_direct) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 

            it "use two states in for loop" $
                (showCode "Compiled: " =<< compileCode Input.two_states_in_loop) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 

            
            it "[ERROR]: returning a literal lacks support" $
                compileCode Input.return_literal `shouldThrow` anyException

            it "Assign and use literal" $
                (showCode "Compiled: " =<< compileCode Input.assign_literal) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.assign_literal
                    compiled `shouldBe` expected) 

            it "Use Binary Operations" $
                (showCode "Compiled: " =<< compileCode Input.binary_operations) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.binary_operations
                    compiled `shouldBe` expected) 

            it "Scope Bindings" $
                (showCode "Compiled: " =<< compileCode Input.scope) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" WIPExpect.scope
                    compiled `shouldBe` expected) 

            it "(For)Loop Scope Bindings" $
                (showCode "Compiled: " =<< compileCode Input.scope_for) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" WIPExpect.scope_for
                    compiled `shouldBe` expected) 

            it "[ERROR] Branch Scope Bindings" $
                compileCode Input.scope_branch `shouldThrow` anyException

            it "Global const in scope" $
            (showCode "Compiled: " =<< compileCode Input.scope_const) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" WIPExpect.scope_const
                    compiled `shouldBe` expected)

            it "Global const in 'all' algos" $
            (showCode "Compiled: " =<< compileCode  Input.scope_static) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" WIPExpect.scope_static
                    compiled `shouldBe` expected)

            it "Error on mutable globals" $
            compileCode  Input.scope_mut `shouldThrow` anyException

            
            
            it "or test" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.or_test) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.kv_application
                compiled `shouldBe` expected)

            -}