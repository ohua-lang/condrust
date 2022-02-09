module Integrations.Python.TailRec where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Python.Utils

import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "TailRec" $ do

        it "ITE/Expr - simple one argument recursion" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)

        it "TODO: ITE/Expr - multiple arguments recursion - This is a compiler error. Please report!" $
            compileCodeWithRec Input.tailRecExpr2 `shouldThrow` anyException
        {-
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecExpr2) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)
        -}

        it "TODO: ITE/Stmt - tailrecursion in if-statements, missing 'return' support" $
            compileCodeWithRec Input.tailRecMultiArg `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecMultiArg) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRecMultiArg
                compiled `shouldBe` expected)-}

        it "TODO: ITE/Stmt contexted function - tailrecursion in if-statements, missing 'return' support" $
            compileCodeWithRec Input.tailRecContext `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecContext) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRecContext
                compiled `shouldBe` expected)
            -}