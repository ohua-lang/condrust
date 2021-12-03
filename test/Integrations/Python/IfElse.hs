module Integrations.Python.IfElse where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "IfElse Expressions/Statements" $ do
       -- The branches are ignored because they only call pure functions and don't 'return' something 
       {- it "Ite/Stmt- Rust Example" $
            (showCode "Compiled: " =<< compileCode Input.iteRustExample) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteRustExample
                compiled `shouldBe` expected)
        -}
         -- This fails because 
        --a) the two "branch tasks" receive thee object and afterwards the control channel is checked
        --   This means there is no gurarntee, that the object is received by the task that should have it 
        -- b) Because the object is not returned from the branches -> similar problem as with loop-return semantiks
        {-
        it "Ite/Stmt- Stateful function" $
            (showCode "Compiled: " =<< compileCode Input.iteStateful) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteRustExample
                compiled `shouldBe` expected)
        -}

        it "Ite/Stmt - Exception on branch 'return'" $
        -- Todo: Replace as soon as we have a real 'unsuported Error'
            compileCode Input.branchReturn `shouldThrow` anyException
    {-
       uncaught exception: ErrorCall
       Internal error: Cannot substitute Binding "x" with a function reference: Lit (FunRefLit (FunRef (QualifiedBinding (NSRef []) (Binding "x")) Nothing Untyped))
       CallStack (from HasCallStack):
        error, called at src/Ohua/Backend/Normalize.hs:88:30 in ohua-backend-0.1.0.0-IU5LaHcdXoz65g8YnoPRFA:Ohua.Backend.Normalize
        -}
        {-
        it "Ite/Stmt - Literal Args" $
            (showCode "Compiled: " =<< compileCode Input.iteLiteralArgs) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteLiteralArgs
                compiled `shouldBe` expected)
        -}
       
        {-
        Tested ob literale als bedingung genutzt werden können

        [Error] Wrong function type 'untyped' for pure function: QualifiedBinding (NSRef [Binding "ohua",Binding "lang"]) (Binding "id")
        CallStack (from HasCallStack):
        throwErrorS, called at src/Ohua/Core/Util.hs:228:20 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Util
        throwErrorDebugS, called at src/Ohua/Core/Monad.hs:39:12 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Monad
        failWith, called at src/Ohua/Core/DFLang/Passes.hs:231:11 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.DFLang.Passes
        -}
    {-
        it "Ite/Expr literal values " $
            (showCode "Compiled: " =<< compileCode Input.condExprLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 
-}
        -- Branch selection before and after the 'if-split' try to receive the decisive varaible
            -- x = something if a else somethingElse
            -- > a is send once, but received twice -> this blocks 
        it "Ite/Expr Rust Example" $
            (showCode "Compiled: " =<< compileCode Input.condExpr2) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr2
                compiled `shouldBe` expected)   
        
        {-uncaught exception: PatternMatchFail
         src/Ohua/Core/ALang/Util.hs:(90,13)-(94,49): Non-exhaustive patterns in case-}
        it "Ite/Expr comparison as condition" $
            (showCode "Compiled: " =<< compileCode Input.condExprComCond) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr2
                compiled `shouldBe` expected) 
        --likewise a 'Pattern match fail in ALang'
        it "Ite/Expr call as condition" $
            (showCode "Compiled: " =<< compileCode Input.condExprComCond) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr2
                compiled `shouldBe` expected)    