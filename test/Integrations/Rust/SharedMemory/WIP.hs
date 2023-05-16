{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.SharedMemory.WIP where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.SharedMemory.Setup
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Data.Position (Span)

spec :: Spec
spec =
    describe "Typing tests" $ do
        it "Type from imported function" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::hello_world;

                fn test() -> String {
                    let x = hello_world();
                    x
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" typeFromExtraction
                compiled `shouldBe` expected)
                
        it "Type from Annotation" $
            (showCode "Compiled: " =<< compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> String {
                    let x: String = not_in_lib();
                    x
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:" typeFromAnnotation                    
                compiled `shouldBe` expected)

        -- In this trivial example we would be able to derive the type anyways. But for now I'll 
        -- have it failing.
        it "Type from Neither" $
            compileCode  [sourceFile|
                use crate::funs::*;

                fn test() -> String {
                    let x = not_in_lib();
                    x
                }
                |] `shouldThrow` anyException


typeFromExtraction:: SourceFile Span 
typeFromExtraction =  [sourceFile|
fn placeholder(){}
|]

typeFromAnnotation:: SourceFile Span 
typeFromAnnotation =  [sourceFile|
fn placeholder(){}
|]