module Integrations.Rust.RustM3.WIPCases where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.RustM3.RustM3Setup
import Integrations.Rust.RustM3.RustTestCode.WIPCode as Input
import Integrations.Rust.RustM3.RustTestCode.WIPOutput as WIPExpect
import Integrations.Rust.RustM3.RustTestCode.BasicsOutput as Expect

spec :: Spec
spec =
    describe "WIP Tests" $ do
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