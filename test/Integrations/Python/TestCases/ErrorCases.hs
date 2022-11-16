{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Integrations.Python.TestCases.ErrorCases where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Setup
import Integrations.Python.CodeSamples.SimpleQuoter
import Language.Python.Common.AST (ModuleSpan)

-- Everthing in here is expected to throw an error or
-- produce invalid Python. As we agreed to keep tests green those tests 
-- either expect exceptions or are tautologies.

spec :: Spec
spec =
    describe "Error Cases from Conditonals in Loops" $ do 
        it "FAIL: Unintilalized literals used in output code" $
            (showCode "Compiled: " =<< compileCode ifElseLoop) >>=
            (\compiled -> do
                expected <- showCode "Expected:" ifElseLoop
                compiled `shouldBe` compiled) 

        it "FAIL: Double used channel in output code" $ 
            (showCode "Compiled: " =<< compileCode ifElseLoop') >>=
            (\compiled -> do
                expected <- showCode "Expected:" ifElseLoop'
                compiled `shouldBe` compiled) 

        it "FAIL: double channel use, inefficient/interesting pattern deconstruction" $ 
            (showCode "Compiled: " =<< compileCode ifElseLoop'') >>=
            (\compiled -> do
                expected <- showCode "Expected:" ifElseLoop''
                compiled `shouldBe` compiled)

        it "ERROR: Usupported output configuration, triple output" $ do
            compileCode ifElseLoop''' `shouldThrow` anyException

        it "FAIL: Object channels are not initialized " $
            (showCode "Compiled: " =<< compileCode ifElseLoopState) >>=
            (\compiled -> do
                expected <- showCode "Expected:" ifElseLoopState
                compiled `shouldBe` compiled) 

        it "ERROR: No support for destructuring for loop data " $ 
            compileCode ifElseLoopState' `shouldThrow` anyException

        it "ERROR: Unsupported multiple outputs, for different methods of object" $
            compileCode ifElseLoopStates `shouldThrow` anyException

        it "ERROR: SMap EnvVarInput is just ignored -> Output Code is wrong" $
            compileCode ifElseLoopStates' `shouldThrow` anyException

        it "FAIL: Because some 'ohua.lang.id(current)' made it into the output code" $
            compileCode ifElseLikeTailRec `shouldThrow` anyException

        it "ERROR: Host expression encountered ... 'This is a compiler error please report'" $
            compileCodeWithRec tailRec' `shouldThrow` anyException

        
        
--- Test Inputs ---------------------------------------

ifElseLoopStates' :: ModuleSpan
ifElseLoopStates' =  [pythonModule|

from helpers.library_proxy import *

def algo(statefulObjs):
    result = []
    for obj in statefulObjs:
        var = obj.method()
        d = fun1(var) if check(var) else fun2(var)
        result.append(d)
    return result 
|]


-- Interesting Error: Variables 'lit_unit_0_1' and 'lit_unit_0_2' are "invented" including according channels
-- Those variables are/should be received before fun1()/fun2() are called. 
-- However they are never calculated or send anywhere 
ifElseLoop :: ModuleSpan
ifElseLoop =  [pythonModule|
from helpers.library_proxy import *


def algo(i):
    result = []
    for j in range(i):
        d = fun1() if check(j) else fun2()
        result.append(d)
    return result 
|]


-- Fliegt weil channel doppelt genutzt wird:
{- PROBLEM:
def task_6(x_0_0_0_sender, y_0_0_0_sender, d_1_receiver):
    while True:
        var_0 = d_1_receiver.recv()
        res = double(var_0)
        x_0_0_0 = res[0]
        x_0_0_0_sender.send(x_0_0_0)
        y_0_0_0 = res[1]
        y_0_0_0_sender.send(y_0_0_0)


def task_7(c_0_0_0_sender, c_0_0_1_sender, d_1_receiver):
    while True:
        var_0 = d_1_receiver.recv()
        res = check(var_0)
        c_0_0_0_sender.send(res)
        c_0_0_1_sender.send(res)
-}
ifElseLoop' :: ModuleSpan
ifElseLoop' =  [pythonModule|
from helpers.library_proxy import *


def algo(i):
    result = []
    for j in range(i):
        x,y = double(j)
        d = fun1(x) if check(j) else fun2(y)
        result.append(d)
    return result 
|]


-- ähnliches Problem wie oben, außerdem ist die Erzeugung von x und y ...öööhm 
{- PROBLEM:
def task_7(x_0_0_0_sender, y_0_0_0_sender, d_1_receiver):
    while True:
        var_0 = d_1_receiver.recv()
        var_1 = d_1_receiver.recv()
        res = var_0, var_1
        x_0_0_0 = res[0]
        x_0_0_0_sender.send(x_0_0_0)
        y_0_0_0 = res[1]
        y_0_0_0_sender.send(y_0_0_0)-}

ifElseLoop'' :: ModuleSpan
ifElseLoop'' =  [pythonModule|
from helpers.library_proxy import *


def algo(i):
    result = []
    for j in range(i):
        x,y = j,j
        d = fun1(x) if check(j) else fun2(y)
        result.append(d)
    return result 
|]

--Error: unsupported output configuration 

ifElseLoop''' :: ModuleSpan
ifElseLoop''' =  [pythonModule|
from helpers.library_proxy import *


def algo(i):
    result = []
    for j in range(i):
        x,y,z = triple(j)
        d = fun1(x) if check(z) else fun2(y)
        result.append(d)
    return result 
|]


-- Very interessting Error: Channels 'obj_0_0_2_3_sender' and 'obj_0_0_2_4_sender' are used (and obvioulsy 
-- also appear in the channels per task) but not intialized. This means they are present in channels per task
-- but not in the channels list of the whole program
ifElseLoopState :: ModuleSpan
ifElseLoopState =  [pythonModule|
from helpers.library_proxy import *


def algo(i):
    obj = MObs()
    result = []
    for j in range(i):
        value = fun1(j)
        d = obj.method() if value == 3 else obj.otherMethod()
        result.append(d)
    return result 
|]


--Error: we currently do not support destructuring and dispatch for loop data
ifElseLoopState' :: ModuleSpan
ifElseLoopState' =  [pythonModule|
from helpers.library_proxy import *


def algo(i):
    result = []
    for j in range(i):
        value = fun1(j)
        d = j.method() if value == 3 else j.otherMethod()
        result.append(d)
    return result 
|]


-- [Error] Unsupported multiple outputs for state on stateful function
ifElseLoopStates :: ModuleSpan
ifElseLoopStates =  [pythonModule|
from helpers.library_proxy import *


def algo(statefulObjs):
    result = []
    for obj in statefulObjs:
        d = obj.method() if obj.check() else obj.otherMethod()
        result.append(d)
    return result 
|]


--Error: Host expression encountered ... 'This is a compiler error please report'
-- The problem is, that I must not call the recursive function directly with parameter 'i'
tailRec' :: ModuleSpan
tailRec' =  [pythonModule|
from helpers.library_proxy import *

def rec(i):
    current = fun1(i)
    c1, c2 = double(current)
    return rec(c2) if check(c2) else c1

def algo(i):
    result = rec(i)
    return result 
|]

-- Error because some 'ohua.lang.id(current)' made it into the output code
ifElseLikeTailRec :: ModuleSpan
ifElseLikeTailRec =  [pythonModule|
from helpers.library_proxy import *

def inner(i):
    c1, c2 = double(i)
    flag = fun1(c1)
    current = fun1(c2)
    return fun2(current) if check(flag) else current

def algo(i):
    result = []
    for j in range(i):
        d = inner(j)
        result.append(d)
    return result
|]