{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.Control where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils


spec :: Spec
spec =
    describe "Control" $ do
        -- FIXME see issue sertel/ohua-core#16
        it "var reuse" $
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile|
                use funs::*;

                fn rec(one:i32, two:i32) -> i32 {
                    let i = h(one);
                    let j = h(two);
                    let k = h2(one, two);
                    if check(i) {
                        rec(i,j)
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
