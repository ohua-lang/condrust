module Integrations.Python.TailRec where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Python.Utils

import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "TailRec" $ do
        -- All produce the interesting error:
        {-
        uncaught exception: ErrorCall
            invariant broken: recursive function does not have the proper structure.
            CallStack (from HasCallStack):
                error, called at src/Ohua/Core/Feature/TailRec/Passes/ALang.hs:447:9 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Feature.TailRec.Passes.ALang
                
        -}
        it "simple one argument" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)
        it "multi-argument" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecMultiArg) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRecMultiArg
                compiled `shouldBe` expected)
        it "contexted function" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecContext) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRecContext
                compiled `shouldBe` expected)