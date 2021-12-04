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
        Produziert fehlerhaften Code i.e. Object wird in Schleife korrekt bearbeitet, aber
        anschließend 1x zu einem Task gesendet, der es n-1 x verwirft/ verwerfen würde
        -}
  
        it "ForLoop over iterator on object" $
            (showCode "Compiled: " =<< compileCode Input.loopIterObj) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterObj
                compiled `shouldBe` expected)
{-
        Produzieren den selben Fehler und zusätzlich eine anderen: Das dict über dessen Items gelooped werden soll, wird zwar erzeugt und empfangen
        aber seine items werden anschließend nicht gesendet so das effective nichts passiert.
        
        it "ForLoop with tuple pattern " $
            (showCode "Compiled: " =<< compileCode Input.loopTuplePattern) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopTuplePattern
                compiled `shouldBe` expected)
-}
        it "ForLoop with comma separated vars" $
            (showCode "Compiled: " =<< compileCode Input.loopCommaPattern) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopCommaPattern
                compiled `shouldBe` expected)
{-
        [Error] Not in scope Binding "i_1"
            CallStack (from HasCallStack):
            throwErrorS, called at src/Ohua/Core/Util.hs:228:20 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Util
            throwErrorDebugS, called at src/Ohua/Core/Monad.hs:39:12 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.Monad
            failWith, called at src/Ohua/Core/ALang/Passes.hs:352:28 in ohua-core-0.3.0-k6lwMxcZZw8E652k33pRb:Ohua.Core.ALang.Passes

--}           
        it "While loop" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.whileLoop) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.whileLoop
                compiled `shouldBe` expected)

