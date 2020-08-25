{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.TypeExtractionSpec
    ( spec
    ) where

import Ohua.Prelude

import Test.Hspec

import Language.Rust.Quote
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Parser (Span)

import qualified Data.HashMap.Lazy as HM

import Ohua.Integration.Rust.TypeExtraction


extractTypes :: Show a => SourceFile a -> IO (HM.HashMap FunRef (FunType a))
extractTypes srcFile = runCompM LevelDebug $ extract "" srcFile

spec :: Spec
spec = 
    describe "type extraction" $ -- do
        it "top-level functions" $
            extractTypes (void [sourceFile| 
                fn void_input() -> String { unimplemented!{}}
                fn void_output(i:i32) { unimplemented!{} }
                fn simple(i:i32) -> String { unimplemented!{} }                
                |]) >>= (`shouldBe` 
                    HM.fromList [
                        ( FunRef (QualifiedBinding (makeThrow []) "void_input") Nothing
                        , FunType [] $ Just $ void [ty| String |]
                        ) ,
                        ( FunRef (QualifiedBinding (makeThrow []) "void_output") Nothing
                        , FunType [ Normal $ void [ty| i32 |] ] Nothing
                        ) ,
                        ( FunRef (QualifiedBinding (makeThrow []) "simple") Nothing
                        , FunType [ Normal $ void [ty| i32 |] ] $ Just $ void [ty| String |]
                        )
                    ]
                )

