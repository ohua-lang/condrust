{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.TypeHandlingSpec
    ( spec
    ) where

import Ohua.Prelude

import Test.Hspec

import Language.Rust.Quote
import Language.Rust.Syntax (SourceFile, Mutability(..))
import Language.Rust.Parser (Span)

import qualified Data.HashMap.Lazy as HM

import Ohua.Integration.Rust.TypeHandling


-- extractTypes :: Show a => SourceFile a -> IO (HM.HashMap QualifiedBinding (FunType (RustVarType)))
-- extractTypes srcFile = runErrAndLogM LevelDebug $ extract "" srcFile

-- ToDo: We don't use this type extraction any more. So they're commented out to avoid conflicts with changes
-- in used code. Replace with appropriate tests. (If there's time before the 'big rewrite')
spec :: Spec
spec =
    describe "type extraction" $ do
        it "placeholder test" $
            1 `shouldBe` 1
        {-
        it "top-level functions" $
            extractTypes (void [sourceFile|
                fn void_input() -> String { unimplemented!{}}
                fn void_output(i:i32) { unimplemented!{} }
                fn simple(i:i32) -> String { unimplemented!{} }
                |]) >>= (`shouldBe`
                    HM.fromList [
                        ( QualifiedBinding (makeThrow []) "void_input"
                        , FunType $ Left Unit --  Just $ void [ty| String |]
                        ) ,
                        ( QualifiedBinding (makeThrow []) "void_output"
                        , FunType $ Right ((Type $ Normal $ void [ty| i32 |]) :| []) -- Nothing
                        ) ,
                        ( QualifiedBinding (makeThrow []) "simple"
                        , FunType $ Right ((Type $ Normal $ void [ty| i32 |]) :| []) -- Just $ void [ty| String |]
                        )
                    ]
                )
        it "impl functions" $
            extractTypes (void [sourceFile|
                impl Foo {
                    fn void_input() -> String { unimplemented!{}}
                    fn void_output(i:i32) { unimplemented!{} }
                    fn simple(i:i32) -> String { unimplemented!{} }
                    fn simple_self(self, i:i32) -> String { unimplemented!{} }
                    fn simple_mut_self(mut self, i:i32) -> String { unimplemented!{} }
                }
                |]) >>= (`shouldBe`
                    HM.fromList [
                        ( QualifiedBinding (makeThrow []) "void_input"
                        , FunType $ Left Unit  -- Just $ void [ty| String |]
                        ) ,
                        ( QualifiedBinding (makeThrow []) "void_output"
                        , FunType $ Right ((Type $ Normal $ void [ty| i32 |]) :| []) -- Nothing
                        ) ,
                        ( QualifiedBinding (makeThrow []) "simple"
                        , FunType $ Right ((Type $ Normal $ void [ty| i32 |]) :| []) --  Just $ void [ty| String |]
                        ) ,
                        ( QualifiedBinding (makeThrow []) "simple_self"
                        , STFunType
                            (Type $ Self (void [ty| Foo |]) Nothing Immutable)
                            $ Right ((Type $ Normal $ void [ty| i32 |]) :| []) -- Just $ void [ty| String |]
                        ),
                        ( QualifiedBinding (makeThrow []) "simple_mut_self"
                        , STFunType
                            (Type $ Self (void [ty| Foo |]) Nothing Mutable)
                            $ Right ((Type $ Normal $ void [ty| i32 |]) :| []) --  Just $ void [ty| String |]
                        )
                    ]
                )
        -- it "mod functions" $
        --     extractTypes (void [sourceFile|
        --         mod Foo {
        --             fn void_input() -> String { unimplemented!{}}
        --             fn void_output(i:i32) { unimplemented!{} }
        --             fn simple(i:i32) -> String { unimplemented!{} }
        --         }
        --         |]) >>= (`shouldBe` 
        --             HM.fromList [
        --                 ( FunRef (QualifiedBinding (makeThrow []) "void_input") 
        --                 , FunType [] $ Just $ void [ty| String |]
        --                 ) ,
        --                 ( FunRef (QualifiedBinding (makeThrow []) "void_output") 
        --                 , FunType [ Normal $ void [ty| i32 |] ] Nothing
        --                 ) ,
        --                 ( FunRef (QualifiedBinding (makeThrow []) "simple") 
        --                 , FunType [ Normal $ void [ty| i32 |] ] $ Just $ void [ty| String |]
        --                 )
        --             ]
        --         )
        -- it "trait functions" $
        --     extractTypes (void [sourceFile|
        --         trait Foo {
        --             fn void_input() -> String { unimplemented!{}}
        --             fn void_output(i:i32) { unimplemented!{} }
        --             fn simple(i:i32) -> String { unimplemented!{} }
        --             fn simple_self(self, i:i32) -> String { unimplemented!{} }
        --             fn simple_mut_self(mut self, i:i32) -> String { unimplemented!{} }
        --         }
        --         |]) >>= (`shouldBe` 
        --             HM.fromList [
        --                 ( FunRef (QualifiedBinding (makeThrow []) "void_input")
        --                 , FunType [] $ Just $ void [ty| String |]
        --                 ) ,
        --                 ( FunRef (QualifiedBinding (makeThrow []) "void_output") 
        --                 , FunType [ Normal $ void [ty| i32 |] ] Nothing
        --                 ) ,
        --                 ( FunRef (QualifiedBinding (makeThrow []) "simple") 
        --                 , FunType [ Normal $ void [ty| i32 |] ] $ Just $ void [ty| String |]
        --                 ),
        --                 ( FunRef (QualifiedBinding (makeThrow []) "simple_self") 
        --                 , FunType [Self (void [ty| Foo |]) Immutable, Normal $ void [ty| i32 |] ] $ Just $ void [ty| String |]
        --                 ),
        --                 ( FunRef (QualifiedBinding (makeThrow []) "simple_mut_self") 
        --                 , FunType [Self (void [ty| Foo |]) Mutable, Normal $ void [ty| i32 |] ] $ Just $ void [ty| String |]
        --                 )
        --             ]
        --         )-}



