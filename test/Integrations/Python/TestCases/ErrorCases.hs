module Integrations.Python.TestCases.ErrorCases where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Setup
import qualified Integrations.Python.CodeSamples.ErrorInputs as Input

-- Everthing in here is expected to throw an error or
-- produce invalid Python. As we agreed to keep tests green those tests 
-- either expect exceptions or are tautologies.

spec :: Spec
spec =
    describe "Error Cases from Conditonals in Loops" $ do 
        it "FAIL: Unintilalized literals used in output code" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoop) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Input.ifElseLoop
                compiled `shouldBe` compiled) 

        it "FAIL: Double used channel in output code" $ 
            (showCode "Compiled: " =<< compileCode Input.ifElseLoop') >>=
            (\compiled -> do
                expected <- showCode "Expected:" Input.ifElseLoop'
                compiled `shouldBe` compiled) 

        it "FAIL: double channel use, inefficient/interesting pattern deconstruction" $ 
            (showCode "Compiled: " =<< compileCode Input.ifElseLoop'') >>=
            (\compiled -> do
                expected <- showCode "Expected:" Input.ifElseLoop''
                compiled `shouldBe` compiled)

        it "ERROR: Usupported output configuration, triple output" $ do
            compileCode Input.ifElseLoop''' `shouldThrow` anyException

        it "FAIL: Object channels are not initialized " $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoopState) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Input.ifElseLoopState
                compiled `shouldBe` compiled) 

        it "ERROR: No support for destructuring for loop data " $ 
            compileCode Input.ifElseLoopState' `shouldThrow` anyException

        it "ERROR: Unsupported multiple outputs, for different methods of object" $
            compileCode Input.ifElseLoopStates `shouldThrow` anyException

        it "ERROR: Can't iterate argument directly; Input to SMap must be var not literal" $
            compileCode Input.ifElseLoopStates' `shouldThrow` anyException

        it "FAIL: Because some 'ohua.lang.id(current)' made it into the output code" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLikeTailRec) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Input.ifElseLikeTailRec
                compiled `shouldBe` compiled) 

        it "ERROR: Host expression encountered ... 'This is a compiler error please report'" $
            compileCode Input.tailRec' `shouldThrow` anyException

        
        
