module Integrations.Python.Loops where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Utils
import qualified Integrations.Python.TestDataInput as Input
import qualified Integrations.Python.TestDataOutput as Expect


spec :: Spec
spec =
    describe "Loop Statements" $ do
        -- Produces teh same error as all testcases that do not return something 
        {-
        uncaught exception: ErrorCall
            Internal Error: Tried running substitution on sending Binding "x" with not yet supported task expression: Lit UnitLit
            CallStack (from HasCallStack):
            error, called at src/Ohua/Backend/Normalize.hs:83:12 in ohua-backend-0.1.0.0-IU5LaHcdXoz65g8YnoPRFA:Ohua.Backend.Normalize
        
        -}
        it "ForLoop over iterator" $
            (showCode "Compiled: " =<< compileCode Input.loopIterator) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterator
                compiled `shouldBe` expected)
        {-
        [Error] Not in scope Binding "i_1"
            CallStack (from HasCallStack):
            throwErrorS, called at src/Ohua/Core/Util.hs:228:20 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Util
            throwErrorDebugS, called at src/Ohua/Core/Monad.hs:39:12 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Monad
            failWith, called at src/Ohua/Core/ALang/Passes.hs:352:28 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.ALang.Passes
        
              
        it "While loop" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.whileLoop) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.whileLoop
                compiled `shouldBe` expected)

--}