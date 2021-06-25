{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.Control where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils


spec :: Spec
spec =
    describe "Control" $
    describe "var reuse" $ do
        -- FIXME see issue sertel/ohua-core#16
        it "simple" $
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use funs::*;

                fn rec(one:i32) -> i32 {
                    let i = h(one);
                    let j = h(one);
                    let k = h2(i, j);
                    if check(k) {
                        rec(k)
                    } else {
                        k
                    }
                }

                fn test() -> i32 {
                    rec(2,4)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                        use funs::*;

                        // TODO
                    |]
                compiled `shouldBe` expected)
        it "dep" $
          -- fusion is more tricky here because `one` is used by data dependent calls!
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use funs::*;

                fn rec(one:i32) -> i32 {
                    let i = h(one);
                    let k = h2(one, i);
                    if check(k) {
                        rec(k)
                    } else {
                        k
                    }
                }

                fn test() -> i32 {
                    rec(2,4)
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                        use funs::*;

                        // TODO
                    |]
                compiled `shouldBe` expected)
