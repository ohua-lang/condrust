module Integrations.Rust.RustM3.KVStoreCase where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )

import Integrations.Rust.RustM3.RustM3Setup
import Integrations.Rust.RustM3.RustTestCode.KVAppCode as Input
import Integrations.Rust.RustM3.RustTestCode.KVAppCompiled as Expect

spec :: Spec
spec =
    describe "KVApplication should compile" $ do
        it "KV-Application, 3 Step Loop" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.kv_application) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.kv_application
                compiled `shouldBe` expected)