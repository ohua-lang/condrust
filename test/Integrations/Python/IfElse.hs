module Integrations.Python.IfElse where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
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
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 
   
        {-uncaught exception: PatternMatchFail
         src/Ohua/Core/ALang/Util.hs:(90,13)-(94,49): Non-exhaustive patterns in case-}
        it "Ite/Expr comparison as condition" $
            (showCode "Compiled: " =<< compileCode Input.condExprComCond) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 
        --likewise a 'Pattern match fail in ALang'
        it "Ite/Expr call as condition" $
            (showCode "Compiled: " =<< compileCode Input.condExprComCond) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected)  
        
        it "Ite/Expr context function" $
            (showCode "Compiled: " =<< compileCode Input.condContextFunction) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condContextFunction
                compiled `shouldBe` expected)
        {-
        Can literals be returned as reult i.e. if a then 1 else 0
        -}

        it "Ite/Expr literal return values " $
            (showCode "Compiled: " =<< compileCode Input.condExprLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 
        
        it "Ite/Expr stateful return values " $
            (showCode "Compiled: " =<< compileCode Input.condExprStateRet) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 

        it "Ite/Stmt - Exception on branch 'return'" $
        -- Todo: Replace as soon as we have a real 'unsuported Error'
            compileCode Input.branchReturn `shouldThrow` anyException


        it "Ite/Stmt - Literal Args" $
            (showCode "Compiled: " =<< compileCode Input.iteLiteralArgs) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteLiteralArgs
                compiled `shouldBe` expected)

       -- This fails because 
        --a) the two "branch tasks" receive thee object and afterwards the control channel is checked
        --   This means there is no gurarntee, that the object is received by the task that should have it 
        -- b) Because the object is not returned from the branches -> similar problem as with loop-return semantiks
    
        it "Ite/Stmt- Stateful function" $
            (showCode "Compiled: " =<< compileCode Input.iteStateful) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteRustExample
                compiled `shouldBe` expected)  

        it "Ite/Stmt -  only if stateless" $
            (showCode "Compiled: " =<< compileCode Input.iteOnlyIf) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 
        

        it "Ite/Stmt -  only if stateful" $
            (showCode "Compiled: " =<< compileCode Input.iteOnlyIfState) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 


      
      