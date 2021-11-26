module Integrations.Python.IfElse where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "IfElse Expressions/Statements" $ do
        -- This fails because code inside branches is ignored 
        -- > I need another mapping of branches to frontend maybe, evaluate after changes in core
        it "Ite/Stmt- Rust Example" $
            (showCode "Compiled: " =<< compileCode Input.iteRustExample) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteRustExample
                compiled `shouldBe` expected)
        
        {-This case produces an interesing problem. The stateful object, manipulated in 
            either of the branches is not passed on i.e. it is manipulated in a loop but no 
            result is send-}
        it "Ite/Stmt- Stateful function" $
            (showCode "Compiled: " =<< compileCode Input.iteStateful) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteRustExample
                compiled `shouldBe` expected)
         
        it "Ite/Stmt - Exception on branch 'return'" $
        -- Todo: Replace as soon as we have a real 'unsuported Error'
            compileCode Input.branchReturn `shouldThrow` anyException
    {-
        [Error] Compiler invariant broken! IfFun arguments don't match:
        PureDFFun (Destruct (Direct (DataBinding (Binding "ctrlTrue_0")) :| [Direct (DataBinding (Binding "ctrlFalse_0"))])) (FunRef (QualifiedBinding (NSRef [Binding "ohua",Binding "lang"]) (Binding "ifFun")) Nothing (FunType (Right (TypeVar :| [])))) (DFEnvVar TypeVar (EnvRefLit (Binding "a")) :| [])  
        -}
        it "Ite/Stmt - Literal Args" $
            (showCode "Compiled: " =<< compileCode Input.iteLiteralArgs) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteLiteralArgs
                compiled `shouldBe` expected)

         {-
        [Error] Compiler invariant broken! IfFun arguments don't match:
        PureDFFun (Destruct (Direct (DataBinding (Binding "ctrlTrue_0")) :| [Direct (DataBinding (Binding "ctrlFalse_0"))])) (FunRef (QualifiedBinding (NSRef [Binding "ohua",Binding "lang"]) (Binding "ifFun")) Nothing (FunType (Right (TypeVar :| [])))) (DFEnvVar TypeVar (EnvRefLit (Binding "a")) :| [])  
        -}
        it "Ite/Stmt - Literal Args" $
            (showCode "Compiled: " =<< compileCode Input.iteLiteralArgs) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteLiteralArgs
                compiled `shouldBe` expected)
       
        {-
        [Error] Wrong function type 'untyped' for pure function: QualifiedBinding (NSRef [Binding "ohua",Binding "lang"]) (Binding "id")
        CallStack (from HasCallStack):
        throwErrorS, called at src/Ohua/Core/Util.hs:228:20 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Util
        throwErrorDebugS, called at src/Ohua/Core/Monad.hs:39:12 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Monad
        failWith, called at src/Ohua/Core/DFLang/Passes.hs:231:11 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.DFLang.Passes
        -}
        it "Ite/Expr literal values " $
            (showCode "Compiled: " =<< compileCode Input.condExprLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 

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
        

       