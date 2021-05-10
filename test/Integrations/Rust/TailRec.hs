{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.TailRec where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils


spec :: Spec
spec = 
    describe "TailRec" $ do
        it "simple one argument" $
            (showCode "Compiled: " =<< compileCodeWithRec [sourceFile| 
                use funs::*;

                fn rec(i:i32) -> i32 {
                    let j = h(i);
                    if check(j) {
                        rec(j)
                    } else {
                        j
                    }
                }

                fn test() -> i32 {
                    rec(2)
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        use funs::*;

                    |]
                compiled `shouldBe` expected)
