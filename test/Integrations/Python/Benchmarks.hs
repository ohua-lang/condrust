module Integrations.Python.Benchmarks where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.PythonSetup
import qualified Integrations.Python.BenchInput as Input
import qualified Integrations.Python.BenchOutput as Expect
import qualified Integrations.Python.NatParOutput as NPExpect



spec :: Spec
spec =
    describe "Testing Benchmmark Comp." $ do 
        {-
        it "Black Scholes" $
            (showCode "Compiled: " =<< compileCode Input.blackScholes) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.blackScholes
                compiled `shouldBe` expected)
        
        it "Natural Parallelism 4 tasks" $
            (showCode "Compiled: " =<< compileCode Input.natPar4) >>=
            (\compiled -> do
                expected <- showCode "Expected:" NPExpect.natPar4
                compiled `shouldBe` expected)
    
        it "Natural Parallelism 8 tasks" $
            (showCode "Compiled: " =<< compileCode Input.natPar8) >>=
            (\compiled -> do
                expected <- showCode "Expected:" NPExpect.natPar8
                compiled `shouldBe` expected)

        it "Natural Parallelism 12 tasks" $
            (showCode "Compiled: " =<< compileCode Input.natPar12) >>=
            (\compiled -> do
                expected <- showCode "Expected:" NPExpect.natPar12
                compiled `shouldBe` expected)

        it "Natural Parallelism 16 tasks" $
            (showCode "Compiled: " =<< compileCode Input.natPar16) >>=
            (\compiled -> do
                expected <- showCode "Expected:" NPExpect.natPar16
                compiled `shouldBe` expected)

        it "Natural Parallelism 20 tasks" $
            (showCode "Compiled: " =<< compileCode Input.natPar20) >>=
            (\compiled -> do
                expected <- showCode "Expected:"  NPExpect.natPar20
                compiled `shouldBe` expected)

        it "Natural Parallelism 24 tasks" $
            (showCode "Compiled: " =<< compileCode Input.natPar24) >>=
            (\compiled -> do
                expected <- showCode "Expected:"  NPExpect.natPar24
                compiled `shouldBe` expected)

        it "Natural Parallelism 28 tasks" $
            (showCode "Compiled: " =<< compileCode Input.natPar28) >>=
            (\compiled -> do
                expected <- showCode "Expected:" NPExpect.natPar28
                compiled `shouldBe` expected)

        it "Natural Parallelism 32" $
            (showCode "Compiled: " =<< compileCode Input.natPar32) >>=
            (\compiled -> do
                expected <- showCode "Expected:" NPExpect.natPar32
                compiled `shouldBe` expected)
-}{-      
        it "For loop 3 pipelined Tasks" $
            (showCode "Compiled: " =<< compileCode Input.loop3) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loop3
                compiled `shouldBe` expected)
      
        it "ifElse-simple" $
            (showCode "Compiled: " =<< compileCode Input.ifElse) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected)
          -}{- 
        it "ifElse-Loop" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoop) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected) 
        it "ifElse-Loop-2" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoop') >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected)

        it "ifElse-Loop-3" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoop'') >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected)
        it "ifElse-Loop-4" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoop''') >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected)

        it "ifElse-Loop-State" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoopState) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected)       
        
        it "ifElse-Loop-State-2" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoopState') >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected) 

        it "ifElse-Loop over list of States -- call methods" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoopStates) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected) 

        it "ifElse-Loop over list of States -- var from method" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoopStates') >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected)

        it "ifElse-Loop over list of States -- additional function around states list" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoopStates'') >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElseLoopStates''
                compiled `shouldBe` expected)

        it "ifElse-Loop create states in loop" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoopStateVar) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected)


        it "ifElse and loop as separate algos" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoopTwoFuns) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected)
        
        it "ifElse double loop var " $
            (showCode "Compiled: " =<< compileCode Input.ifElseLoopThree) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected) -}

        it "ifElse like working tailrec version, two algos" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLikeTailRec) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElseLikeTailRec
                compiled `shouldBe` expected)

        it "ifElse like working tailrec, but with fun calls in both branches" $
            (showCode "Compiled: " =<< compileCode Input.ifElseLikeTailRec') >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElseLikeTailRec'
                compiled `shouldBe` expected)

        it "ifElse like working tailrec, inner algo call should be fused into outer"$
            (showCode "Compiled: " =<< compileCode Input.ifElseLikeTailRec''') >>=
            (\compiled -> do
                expected <- showCode "Expected:" =<< compileCode Input.ifElseLikeTailRec'''
                compiled `shouldBe` expected)
{-
        it "Tailrec fail" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRec) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected)

        it "Tailrec fail 2" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRec') >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected)
        
        it "TailRecWorks" $
            (showCode "Compiled: " =<< compileCodeWithRec Input.tailRecWorks) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.ifElse
                compiled `shouldBe` expected)
    -}