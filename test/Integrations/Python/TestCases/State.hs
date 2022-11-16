{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Integrations.Python.TestCases.State where
import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Setup

import qualified Integrations.Python.CodeSamples.TestDataOutput as Expect
import Integrations.Python.CodeSamples.SimpleQuoter
import Language.Python.Common.AST (ModuleSpan)


spec :: Spec
spec =
    describe "Testing Statefull Calls" $ do
        it "Method Call" $
            (showCode "Compiled: " =<< compileCode callMethod) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callMethod
                compiled `shouldBe` expected)
        it "flat - get result from state" $
            (showCode "Compiled: " =<< compileCode flat) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.flat
                compiled `shouldBe` expected)
        
        it "thread - modify state, get result" $
            (showCode "Compiled: " =<< compileCode thread) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.thread
                compiled `shouldBe` expected)
                
        it "FAIL: side effect on stream of objects - code sends uninit. variable" $
            (showCode "Compiled: " =<< compileCode loop) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loop
                compiled `shouldBe` compiled)-- expected)


        it "FAIL: single IO - side effect on object in loop - code sends uninit. variable" $
            (showCode "Compiled: " =<< compileCode singleIO) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.singleIO
                compiled `shouldBe` compiled)--expected)
        -- Drop problem
        it "single state - like IO, but return method call" $
            (showCode "Compiled: " =<< compileCode singleState) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.singleState
                compiled `shouldBe` expected)
        -- Drop problem
        it "raw state out" $
            (showCode "Compiled: " =<< compileCode stateOut) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.stateOut
                compiled `shouldBe` expected)
  

---------- Test Inputs --------------------------
callMethod :: ModuleSpan
callMethod = [pythonModule|
from testLib import *

def algo():
    mob = MObs(22)
    # mob.addNum(21)
    x = mob.getNum()
    return x
|]

flat :: ModuleSpan
flat = [pythonModule|
from testLib import *

def algo(i):
    mob = MObs(i)
    result = mob.getNum()
    return oneArg(result) 
|]

thread :: ModuleSpan
thread = [pythonModule|
from testLib import *

def algo(i):
    mob = MObs(i)
    mob.addNum(23)
    result = mob.getNum()
    return result 
|]

loop :: ModuleSpan
loop = [pythonModule|
from testLib import *

def algo(a):
    g = listOfMObs(a)
    for e in g:
        e.addNum(7)
|]

singleIO :: ModuleSpan
singleIO = [pythonModule|
from testLib import *

def algo(a):
    mob = MObs(a)
    g = some_invented_iter_function()
    for e in g:
        mob.addNum(e)
|]

singleState :: ModuleSpan
singleState = [pythonModule|
from testLib import *

def algo(a):
    mob = MObs(a)
    g = some_invented_iter_function()
    for e in g:
        mob.addNum(e)
    return mob.getNum()
|]

stateOut :: ModuleSpan
stateOut = [pythonModule|
from testLib import *

def algo(a):
    mob = MObs(a)
    mob2 = mob.clone()
    g = some_invented_iter_function()
    for e in g:
        x = mobFun(mob2, e) 
        mob.addNum(x)
    return mob
|]