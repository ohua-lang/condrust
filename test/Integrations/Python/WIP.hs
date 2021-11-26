module Integrations.Python.WIP where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect



spec :: Spec
spec =
    describe "WIP Tests" $ do
        {-
        it "Two Algos" $
            (showCode "Compiled: " =<< compileCode Input.twoAlgos) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callMethod
                compiled `shouldBe` expected)

        
        it "Conditional Expression with literals" $
            (showCode "Compiled: " =<< compileCode Input.condExprLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprLit
                compiled `shouldBe` expected)  


        it "Tail Recursive with If-Expr" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)
        
        it "Tail Recursive with If-Stmt" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecStmt) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)

        {-Todo: Error : unitFun must only have one output
        it "Multiassignment comma separated" $
            (showCode "Compiled: " =<< compileCode Input.multiAssignment) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multiAssignment
                compiled `shouldBe` expected)
         -}
    -}  it "Ite/Stmt- Stateful function" $
                (showCode "Compiled: " =<< compileCode Input.iteStateful) >>=
                (\compiled -> do
                    expected <- showCode "Expected:" Expect.iteRustExample
                    compiled `shouldBe` expected)
            
        