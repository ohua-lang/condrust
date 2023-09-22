{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Integrations.Python.TestCases.ErrorCases where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<))

import Integrations.TestSetup
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
                _expected <- showCode "Expected:" ifElseLoop
                compiled `shouldBe` compiled) 

        it "FAIL: Double used channel in output code" $ 
            (showCode "Compiled: " =<< compileCode ifElseLoop') >>=
            (\compiled -> do
                _expected <- showCode "Expected:" ifElseLoop'
                compiled `shouldBe` compiled) 

        it "FAIL: double channel use, inefficient/interesting pattern deconstruction" $ 
            (showCode "Compiled: " =<< compileCode ifElseLoop'') >>=
            (\compiled -> do
                _expected <- showCode "Expected:" ifElseLoop''
                compiled `shouldBe` compiled)


        it "FAIL: Object channels are not initialized " $
            (showCode "Compiled: " =<< compileCode ifElseLoopState) >>=
            (\compiled -> do
                _expected <- showCode "Expected:" ifElseLoopState
                compiled `shouldBe` compiled) 

        it "ERROR: No support for destructuring for loop data " $ 
            compileCode ifElseLoopState' `shouldThrow` anyException

        it "ERROR: Unsupported multiple outputs, for different methods of object" $
            compileCode loopEnvVarBranching `shouldThrow` anyException


        it "return variable" $
            (showCode "Compiled" =<< compileCode ifElseLikeTailRec) >>=
            (\compiled -> do
                _expected <- showCode "Expected:" ifElseLikeTailRecExpected
                compiled `shouldBe` compiled)

        it "ERROR: Host expression encountered ... 'This is a compiler error please report'" $
            compileCodeWithRec tailRec' `shouldThrow` anyException

        
        
--- Test Inputs ---------------------------------------



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



loopEnvVarBranching :: ModuleSpan
loopEnvVarBranching =  [pythonModule|
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

ifElseLikeTailRecExpected :: ModuleSpan
ifElseLikeTailRecExpected =  [pythonModule|
import multiprocessing as mp
from helpers.library_proxy import *
def task_1(g_0_0_sender, current_0_0_0_3_receiver, ctrlFalse_0_receiver):
    while True:
        renew = False
        current_0_0_0_1 = current_0_0_0_3_receiver.recv()
        while not renew:
            sig = ctrlFalse_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                g_0_0_sender.send(current_0_0_0_1)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_2(e_0_0_sender, current_0_0_0_2_receiver, ctrlTrue_0_receiver):
    while True:
        renew = False
        current_0_0_0_0 = current_0_0_0_2_receiver.recv()
        while not renew:
            sig = ctrlTrue_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                e_0_0 = fun2(current_0_0_0_0)
                e_0_0_sender.send(e_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_3(result_2_sender, c_0_0_1_receiver, e_0_0_receiver, g_0_0_receiver):
    while True:
        branchSelection = c_0_0_1_receiver.recv()
        if branchSelection:
            result = e_0_0_receiver.recv()
            result_2_sender.send(result)
        else:
            result = g_0_0_receiver.recv()
            result_2_sender.send(result)
def task_4(ctrlTrue_0_sender, ctrlFalse_0_sender, c_0_0_0_receiver):
    while True:
        branchSelection = c_0_0_0_receiver.recv()
        if branchSelection:
            ctrlTrue = True, 1
            ctrlFalse = True, 0
            ctrlTrue_0_sender.send(ctrlTrue)
            ctrlFalse_0_sender.send(ctrlFalse)
        else:
            ctrlTrue = True, 0
            ctrlFalse = True, 1
            ctrlTrue_0_sender.send(ctrlTrue)
            ctrlFalse_0_sender.send(ctrlFalse)
def task_5(c_0_0_0_sender, c_0_0_1_sender, flag_0_0_0_receiver):
    while True:
        var_0 = flag_0_0_0_receiver.recv()
        res = check(var_0)
        c_0_0_0_sender.send(res)
        c_0_0_1_sender.send(res)
def task_6(current_0_0_0_2_sender, current_0_0_0_3_sender, c2_0_0_0_receiver):
    while True:
        var_0 = c2_0_0_0_receiver.recv()
        res = fun1(var_0)
        current_0_0_0_2_sender.send(res)
        current_0_0_0_3_sender.send(res)
def task_7(flag_0_0_0_sender, c1_0_0_0_receiver):
    while True:
        var_0 = c1_0_0_0_receiver.recv()
        flag_0_0_0 = fun1(var_0)
        flag_0_0_0_sender.send(flag_0_0_0)
def task_8(c1_0_0_0_sender, c2_0_0_0_sender, d_1_receiver):
    while True:
        var_0 = d_1_receiver.recv()
        res = double(var_0)
        c1_0_0_0 = res[0]
        c1_0_0_0_sender.send(c1_0_0_0)
        c2_0_0_0 = res[1]
        c2_0_0_0_sender.send(c2_0_0_0)
def task_9(ctrl_0_0_sender, d_1_sender):
    b_0_0 = range(i)
    hasSize = True if hasattr(b_0_0, '__len__') else False
    if hasSize:
        size = len(b_0_0)
        ctrl = True, size
        ctrl_0_0_sender.send(ctrl)
        for d in b_0_0:
            d_1_sender.send(d)
    else:
        size = 0
        for d in b_0_0:
            d_1_sender.send(d)
            ctrl = False, 1
            ctrl_0_0_sender.send(ctrl)
            size = size + 1
        ctrl = True, 0
        ctrl_0_0_sender.send(ctrl)
def task_10(result_0_0_1_sender):
    result_0_0_1 = []
    result_0_0_1_sender.send(result_0_0_1)
def task_11(result_0_1_0_sender, result_0_0_1_receiver, ctrl_0_0_receiver, result_2_receiver):
    while True:
        renew = False
        result_0_0_1_0 = result_0_0_1_receiver.recv()
        while not renew:
            sig = ctrl_0_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                var_1 = result_2_receiver.recv()
                result_0_0_1_0.append(var_1)
            renew_next_time = sig[0]
            renew = renew_next_time
        result_0_1_0_sender.send(result_0_0_1_0)
def main(i_1):
    global i
    i, = i_1,
    result_0_1_0_sender, result_0_1_0_receiver = mp.Pipe()
    result_0_0_1_sender, result_0_0_1_receiver = mp.Pipe()
    ctrl_0_0_sender, ctrl_0_0_receiver = mp.Pipe()
    d_1_sender, d_1_receiver = mp.Pipe()
    c1_0_0_0_sender, c1_0_0_0_receiver = mp.Pipe()
    c2_0_0_0_sender, c2_0_0_0_receiver = mp.Pipe()
    flag_0_0_0_sender, flag_0_0_0_receiver = mp.Pipe()
    c_0_0_0_sender, c_0_0_0_receiver = mp.Pipe()
    current_0_0_0_2_sender, current_0_0_0_2_receiver = mp.Pipe()
    ctrlTrue_0_sender, ctrlTrue_0_receiver = mp.Pipe()
    current_0_0_0_3_sender, current_0_0_0_3_receiver = mp.Pipe()
    ctrlFalse_0_sender, ctrlFalse_0_receiver = mp.Pipe()
    g_0_0_sender, g_0_0_receiver = mp.Pipe()
    e_0_0_sender, e_0_0_receiver = mp.Pipe()
    c_0_0_1_sender, c_0_0_1_receiver = mp.Pipe()
    result_2_sender, result_2_receiver = mp.Pipe()
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9, task_10, task_11]
    channels = [[g_0_0_sender, current_0_0_0_3_receiver, ctrlFalse_0_receiver], [e_0_0_sender, current_0_0_0_2_receiver, ctrlTrue_0_receiver], [result_2_sender, c_0_0_1_receiver, e_0_0_receiver, g_0_0_receiver], [ctrlTrue_0_sender, ctrlFalse_0_sender, c_0_0_0_receiver], [c_0_0_0_sender, c_0_0_1_sender, flag_0_0_0_receiver], [current_0_0_0_2_sender, current_0_0_0_3_sender, c2_0_0_0_receiver], [flag_0_0_0_sender, c1_0_0_0_receiver], [c1_0_0_0_sender, c2_0_0_0_sender, d_1_receiver], [ctrl_0_0_sender, d_1_sender], [result_0_0_1_sender], [result_0_1_0_sender, result_0_0_1_receiver, ctrl_0_0_receiver, result_2_receiver]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_1_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]



