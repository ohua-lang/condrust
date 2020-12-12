{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SMap where

import Ohua.Prelude

import Integrations.Rust.Utils


spec :: Spec
spec = 
    describe "SMap" $ do
        it "stream" $
            (showCode "Compiled: " =<< compileCode [sourceFile| 
                use funs::*;

                fn test() -> () {
                    let stream = iter();
                    for e in stream {
                        k(e);
                    }
                }
                |]) >>= 
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile| 
                        use funs::*;

                    |]
                compiled `shouldBe` expected)
