module Integrations.Rust.M3.KVStoreCase where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.M3.Setup
import Integrations.Rust.M3.TestCode.KVAppCode as Input
import Integrations.Rust.M3.TestCode.KVAppCompiled as Expect

spec :: Spec
spec =
    describe "KVApplication should compile" $ do
        it "two-component base case" $
            (showCode "Compiled: " =<< compileCodeWithRecWithDebug Input.k_v_application_1_2_components) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.k_v_application_1_2_components
                compiled `shouldBe` expected)

        it "latest version " $
            (showCode "Compiled: " =<< compileCodeWithRec Input.k_v_latest) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.k_v_latest
                compiled `shouldBe` expected) --`shouldThrow` anyException
        {-
        it "if_else_in_rec" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.if_else_in_rec) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.k_v_latest
                compiled `shouldBe` expected) --`shouldThrow` anyException
        -}
        it "just rec" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.just_rec) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.k_v_latest
                compiled `shouldBe` expected) --`shouldThrow` anyException
