module Integrations.Python.IfElse where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "IfElse Expressions/Statements" $ do
        -- This fails for two reasons:
            --1. This one is definitely on me -> I need to introduce parameters for the new main
            --2. Function returns 'oneArg(d)' and 'd' is only assigned inside the branches 
            -- > anything  but  'oneArg(d)' is ignored in the final code
        it "ITE - Rust Example" $
            (showCode "Compiled: " =<< compileCode Input.iteRustExample) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteRustExample
                compiled `shouldBe` expected)
        -- again everything except the return is ignored
        it "Simple ite - Params as Args" $
            (showCode "Compiled: " =<< compileCode Input.iteParamArgs) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteParamArgs
                compiled `shouldBe` expected)
        -- again everything except the return is ignored
        it "Simple ite - No Params" $
            (showCode "Compiled: " =<< compileCode Input.iteNoParams) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteNoParams
                compiled `shouldBe` expected)
    
        -- Here I get the 'None Return Problem again'
        it "Conditional Expression" $
            (showCode "Compiled: " =<< compileCode Input.condExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected)     
        

        -- Same Problem as before:
        -- Branch selelction before annd after the 'if-split' try to receive the decisive varaible
            -- x = something if a else somethingElse
            -- > a is send once, but received twice -> this blocks 
        it "Rust Example as Cond. Expression" $
            (showCode "Compiled: " =<< compileCode Input.condExpr2) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr2
                compiled `shouldBe` expected)     

       
        {-
        [Error] Compiler invariant broken! IfFun arguments don't match:
        PureDFFun (Destruct (Direct (DataBinding (Binding "ctrlTrue_0")) :| [Direct (DataBinding (Binding "ctrlFalse_0"))])) (FunRef (QualifiedBinding (NSRef [Binding "ohua",Binding "lang"]) (Binding "ifFun")) Nothing (FunType (Right (TypeVar :| [])))) (DFEnvVar TypeVar (EnvRefLit (Binding "a")) :| [])  
        -}
        it "Simple ite - Literal Args" $
            (showCode "Compiled: " =<< compileCode Input.iteLiteralArgs) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteLiteralArgs
                compiled `shouldBe` expected)
        
        -- Fails because  only the return call
            -- 'return oneArg(d)' get's translated to a task, which obviously fails throug a lack of 'd'
        it "return in else branch...I think this should fail" $
            (showCode "Compiled: " =<< compileCode Input.branchReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.branchReturn
                compiled `shouldBe` expected) 