module Integrations.Rust.M3.KVStoreCase where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.M3.Setup
import Integrations.Rust.M3.TestCode.KVAppCode as Input
import Integrations.Rust.M3.TestCode.KVAppCompiled as Expect

spec :: Spec
spec =
    describe "KVApplication should compile" $ do
        it "two-component base case" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.k_v_application) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.k_v_application
                compiled `shouldBe` expected)

-- **********************************************************************************************
-- + From here on it's test cases for the old k-v-code
-- **********************************************************************************************
        {-
        it "FAIL: KV-Application using a closure - THIS PRODUCES INVALID OUTPUT " $
            (showCode "Compiled: " =<< compileCodeWithRec Input.kv_application_closure) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.kv_application
                compiled `shouldBe` expected)
        
        {- I can not return unit () from a recursion -}
        it "FAIL: KV-Application using recursion in separate function - CAN NOT RETURN UNIT " $
            (showCode "Compiled: " =<< compileCodeWithRec Input.kv_application_extra_function_return_unit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.kv_application
                compiled `shouldBe` expected)-}

        {- now it does not like the output configuration of poll ?! -}

{-
        it "FAIL: KV-Application using recursion in separate function, returning socket state - UNSUPPORTED OUTPUT CONFIGURATION " $
            (showCode "Compiled: " =<< compileCodeWithRec Input.kv_application_extra_function_return_sockets) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.kv_application
                compiled `shouldBe` expected)



        it "poll only egress - first try" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.poll_only_egress) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.kv_application
                compiled `shouldBe` expected)
-}

    
