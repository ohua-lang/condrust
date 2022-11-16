{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.TestCases.Loops where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Setup

import qualified Integrations.Python.CodeSamples.TestDataOutput as Expect
import Integrations.Python.CodeSamples.SimpleQuoter
import Language.Python.Common.AST (ModuleSpan)

spec :: Spec
spec =
    describe "Loop Statements" $ do
        it "ForLoop over iterator" $
            (showCode "Compiled: " =<< compileCode loopIterator) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterator
                compiled `shouldBe` expected)

        it "ForLoop over iterator an object" $
            (showCode "Compiled: " =<< compileCode loopIterObj) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterObj
                compiled `shouldBe` expected)
        
        it "For loop 3 pipelined Tasks" $
            (showCode "Compiled: " =<< compileCode loop3) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loop3
                compiled `shouldBe` expected)

        it "ERROR: Nested For-Loop, updating State"$
            compileCode nested `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCode Input.nested) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterObj
                compiled `shouldBe` expected)
            -}

        it "FAIL: ForLoop with tuple pattern - Tuple not send to looping task " $
            (showCode "Compiled: " =<< compileCode loopTuplePattern) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopTuplePattern
                compiled `shouldBe` compiled)


        it "ERROR: ForLoop with comma separated vars" $
            compileCode loopCommaPattern `shouldThrow` anyException
        {-
            (showCode "Compiled: " =<< compileCode Input.loopCommaPattern) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopCommaPattern
                compiled `shouldBe` expected)
        -}
      
        it "ERROR: While loop" $
            compileCodeWithRec whileLoop `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCodeWithRec Input.whileLoop) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.whileLoop
                compiled `shouldBe` expected)
            -}

--- Test Inputs ----- 

loopIterator :: ModuleSpan 
loopIterator = [pythonModule|
from testLib import *
def algo():
    s = MObs(42)
    stream = some_invented_iter_function()
    for e in stream:
        r = h(e)
        s.addNum(r)
    return s
|]

loopIterObj :: ModuleSpan
loopIterObj= [pythonModule|
from testLib import *
def algo(a):
    g = dict()
    mOb = MObs(42)
    for i in g.values():
        n = f(i)
        mOb.addNum(n)
    return mOb
|]

loop3 :: ModuleSpan
loop3 = [pythonModule|
from helpers.library_proxy import *


def algo(i):
    result = []
    for j in range(0, i):
        x = fun1(j)
        y = fun2(x)
        z = fun3(y)
        result.append(z)
    return result
|]

nested :: ModuleSpan
nested = [pythonModule|
from testLib import *

def algo(n):
  stream = Stream(n)
  store = Store()
  for worklist in stream:
    for item in worklist:
      update = handle_message(store.clone(), item)
      store.update(res)
|]


loopTuplePattern :: ModuleSpan
loopTuplePattern = [pythonModule|
from testLib import *
def algo(a):
    g = {1:2, 3:4}
    mOb = MObs(42)
    for (i,j) in g.items():
        n = f(i)
        mOb.addNum(n)
    return mOb
|]

loopCommaPattern :: ModuleSpan
loopCommaPattern= [pythonModule|
from testLib import *

def algo(a):
    g = {1:2, 3:4}
    mOb = MObs(42)
    for i,j in g.items():
        n = f(i)
        mOb.addNum(n)
        mOb.addNum(3)
    return mOb
|]

whileLoop :: ModuleSpan
whileLoop = [pythonModule|
from testLib import *

def algo(a):
    n = iter_32()
    s = newState()
    while n.has_next():
        e = n.next()
        s.gs(e)
    return s
|]



