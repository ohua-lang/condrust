module Integrations.Rust.M3.KVStoreCase where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.M3.Setup
import Integrations.Rust.M3.TestCode.KVAppCode as Input
import Integrations.Rust.M3.TestCode.KVAppCompiled as Expect

spec :: Spec
spec =
    describe "KVApplication should compile" $ do
        it "two-component base case" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.k_v_application_1_2_components) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.k_v_application_1_2_components
                compiled `shouldBe` expected)
{-
        it "poll outer loop as recursion" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.k_v_application_2_poll_loop_rec) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.k_v_application_1_2_components
                compiled `shouldBe` expected) --`shouldThrow` anyException
-}