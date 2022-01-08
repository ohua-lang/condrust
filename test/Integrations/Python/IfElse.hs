module Integrations.Python.IfElse where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.PythonSetup
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "IfElse Expressions/Statements" $ do

        it "Ite/Expr simple stateless condition" $
            (showCode "Compiled: " =<< compileCode Input.condExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 
        
        it "Ite/Expr stateful condition" $
            (showCode "Compiled: " =<< compileCode Input.condExprState) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprState
                compiled `shouldBe` expected) 
   
        {-uncaught exception: PatternMatchFail
         src/Ohua/Core/ALang/Util.hs:(90,13)-(94,49): Non-exhaustive patterns in case-}
        it "ERROR: ALang Pattern Match Fail - Ite/Expr comparison as condition" $
            compileCode Input.condExprComCond `shouldThrow` anyException
        {-  (showCode "Compiled: " =<< compileCode Input.condExprComCond) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) -}

        --likewise a 'Pattern match fail in ALang'
        it "Ite/Expr call as condition" $
            (showCode "Compiled: " =<< compileCode Input.condExprFunCond) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprFunCond
                compiled `shouldBe` expected)  

        
        it "Ite/Expr context function" $
            (showCode "Compiled: " =<< compileCode Input.condContextFunction) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condContextFunction
                compiled `shouldBe` expected)
        {-
        Can literals be returned as reult i.e. if a then 1 else 0
        -}

        it "ERROR: Ite/Expr literal return values " $
            compileCode Input.condExprLit `shouldThrow` anyException
        {-  (showCode "Compiled: " =<< compileCode Input.condExprLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) -}
        
        it "Ite/Expr state-function return values " $
            (showCode "Compiled: " =<< compileCode Input.condExprStateFunRet) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprStateFunRet
                compiled `shouldBe` expected) 

        it "Ite/Stmt - Exception on branch 'return'" $
        -- Todo: Replace as soon as we have a real 'unsuported Error'
            compileCode Input.branchReturn `shouldThrow` anyException


        it "ERROR: Ite/Stmt - Literal Args" $
            compileCode Input.iteLiteralArgs `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCode Input.iteLiteralArgs) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteLiteralArgs
                compiled `shouldBe` expected)
            -}
       
        it "ERROR: Ite/Stmt- Stateful function" $
            compileCode Input.iteStateful `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCode Input.iteStateful) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteRustExample
                compiled `shouldBe` expected)  
            -}

        it "ERROR: Ite/Stmt -  only if stateless" $
            compileCode Input.iteOnlyIf `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCode Input.iteOnlyIf) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 
            -}

        it "ERROR; Ite/Stmt -  only if stateful" $
            compileCode Input.iteOnlyIf `shouldThrow` anyException
        {-
            (showCode "Compiled: " =<< compileCode Input.iteOnlyIfState) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 
        -}


      
      