{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Integrations.Python.TestCases.WIP where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Setup 
import qualified Integrations.Python.CodeSamples.TestDataOutput as Expect
import Integrations.Python.CodeSamples.SimpleQuoter
import Language.Python.Common.AST (ModuleSpan)

spec :: Spec
spec =
    describe "WIP Tests" $ do
        it "GlobRef" $
            (showCode "Compiled: " =<< compileCode globalRef) >>=
            (\compiled -> do
                expected <- showCode "Expected:" globalRefOut
                compiled `shouldBe` expected)

        it "Destruct- Base case 2 vars from stateles fun" $
            (showCode "Compiled: " =<< compileCode destruct_2) >>=
            (\compiled -> do
                expected <- showCode "Expected:" destruct_2_out
                compiled `shouldBe` expected)

        it "Destruct - Multi simpl, 4 vars from stateles fun" $
            (showCode "Compiled: " =<< compileCode destruct_4) >>=
            (\compiled -> do
                expected <- showCode "Expected:" destruct_4_out
                compiled `shouldBe` expected)

        it "Destruct - 3 outputs, only 2 used " $
            (showCode "Compiled: " =<< compileCode destruct_3_use_2) >>=
            (\compiled -> do
                expected <- showCode "Expected:" destruct_3_use_2_out
                compiled `shouldBe` expected)

        it "Destruct - 3 outputs used in different scopes, one as state" $
            (showCode "Compiled: " =<< compileCode destruct_3_use_in_loop) >>=
            (\compiled -> do
                expected <- showCode "Expected:" destruct_3_use_in_loop_out 
                compiled `shouldBe` expected)

        {-

        it "Assignment binary Operation of integer literals" $
            (showCode "Compiled: " =<< compileCode assignBinOp) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOp
                compiled `shouldBe` expected)
        it "Accept keyword and default args" $
            (showCode "Compiled: " =<< compileCode argsAndKwargs) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.argsAndKwargs
                compiled `shouldBe` expected)
        

        it "Two Algos" $
            (showCode "Compiled: " =<< compileCode twoAlgos) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callMethod
                compiled `shouldBe` expected)

        
        it "Conditional Expression with literals" $
            (showCode "Compiled: " =<< compileCode condExprLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprLit
                compiled `shouldBe` expected)  


        it "Tail Recursive with If-Expr" $
            (showCode "Compiled: " =<< compileCodeWithRec tailRecExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)
        
        it "Tail Recursive with If-Stmt" $
            (showCode "Compiled: " =<< compileCodeWithRec tailRecStmt) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)

        {-Todo: Error : unitFun must only have one output
        it "Multiassignment comma separated" $
            (showCode "Compiled: " =<< compileCode multiAssignment) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multiAssignment
                compiled `shouldBe` expected)
         -}
        

        it "test args" $
            (showCode "Compiled: " =<< compileCode exprArg) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callAFunction
                compiled `shouldBe` expected)
                

    it "ForLoop over iterator" $
            (showCode "Compiled: " =<< compileCode loopIterator) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterator
                compiled `shouldBe` expected)
        

            -}

----- Test Inputs -----------------
globalRef :: ModuleSpan
globalRef = [pythonModule|
from testLib import *
GLOB = 42

def algo():
    x = fun(GLOB)
|]

twoAlgos :: ModuleSpan
twoAlgos = [pythonModule|
from testLib import *

def algo():
    mob = MObs(22)
    # mob.addNum(21)
    x = mob.getNum()
    return x

def algo2():
    x = 5
    y = algo1() 
    return x + y
|]

argsAndKwargs :: ModuleSpan
argsAndKwargs = [pythonModule|
from testLib import *


def algo(a:str= "hihi", b:int=0):
    x = f(a,b)
    return x
|]

destruct_2 :: ModuleSpan
destruct_2 = [pythonModule|
from testLib import *


def algo(a:str= "hihi"):
    x,y = split_half(a);
    return x
|]


destruct_4 :: ModuleSpan
destruct_4 = [pythonModule|
from testLib import *


def algo(a:str= "hihi"):
    z,y,x,w = split(a);
    return x
|]


destruct_3_use_2 :: ModuleSpan
destruct_3_use_2  = [pythonModule|
from testLib import *

def algo(a:str):
    z,y,x = split(a)
    first = f(z)
    second = g(x)
    return h(first, second)
|]

destruct_3_use_in_loop :: ModuleSpan
destruct_3_use_in_loop  = [pythonModule|
from testLib import *

def algo(a:str):
    first = split(a)
    z,y,x = destruct(first)
    for i in z:
        third = f(y)
        x.do_stuff(third)
    return x
|]


--- Test Outputs ---------------------



globalRefOut :: ModuleSpan
globalRefOut = [pythonModule|
import multiprocessing as mp
from testLib import *
GLOB = 42
def task_1(a_0_0_sender, x_0_0_0_receiver):
    x_0_0_0_receiver.recv()
    a_0_0_sender.send(None)
def task_2(x_0_0_0_sender):
    x_0_0_0 = fun(GLOB)
    x_0_0_0_sender.send(x_0_0_0)
def main():
    a_0_0_sender, a_0_0_receiver = mp.Pipe()
    x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
    tasks = [task_1, task_2]
    channels = [[a_0_0_sender, x_0_0_0_receiver], [x_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = a_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result 
|]

destruct_2_out :: ModuleSpan
destruct_2_out = [pythonModule|
import multiprocessing as mp
from testLib import *
def task_1(x_0_0_0_sender, y_0_0_0_sender):
    res = split_half(a)
    x_0_0_0 = res[0]
    x_0_0_0_sender.send(x_0_0_0)
    y_0_0_0 = res[1]
    y_0_0_0_sender.send(y_0_0_0)
def main(a_1:str ="hihi"):
    global a
    a, = a_1,
    x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
    tasks = [task_1]
    channels = [[x_0_0_0_sender, y_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result 
|]

destruct_4_out :: ModuleSpan
destruct_4_out = [pythonModule|
import multiprocessing as mp
from testLib import *
def task_1(z_0_0_0_sender, y_0_0_0_sender, x_0_0_0_sender, w_0_0_0_sender):
    res = split(a)
    z_0_0_0 = res[0]
    z_0_0_0_sender.send(z_0_0_0)
    y_0_0_0 = res[1]
    y_0_0_0_sender.send(y_0_0_0)
    x_0_0_0 = res[2]
    x_0_0_0_sender.send(x_0_0_0)
    w_0_0_0 = res[3]
    w_0_0_0_sender.send(w_0_0_0)
def main(a_1:str ="hihi"):
    global a
    a, = a_1,
    x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
    tasks = [task_1]
    channels = [[z_0_0_0_sender, y_0_0_0_sender, x_0_0_0_sender, w_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result 
|]



destruct_3_use_2_out :: ModuleSpan
destruct_3_use_2_out  = [pythonModule|
import multiprocessing as mp
from testLib import *
def task_1(c_0_0_sender, first_0_0_0_receiver, second_0_0_0_receiver):
    while True:
        var_0 = first_0_0_0_receiver.recv()
        var_1 = second_0_0_0_receiver.recv()
        c_0_0 = h(var_0, var_1)
        c_0_0_sender.send(c_0_0)
def task_2(second_0_0_0_sender, x_0_0_0_receiver):
    while True:
        var_0 = x_0_0_0_receiver.recv()
        second_0_0_0 = g(var_0)
        second_0_0_0_sender.send(second_0_0_0)
def task_3(first_0_0_0_sender, z_0_0_0_receiver):
    while True:
        var_0 = z_0_0_0_receiver.recv()
        first_0_0_0 = f(var_0)
        first_0_0_0_sender.send(first_0_0_0)
def task_4(z_0_0_0_sender, y_0_0_0_sender, x_0_0_0_sender):
    res = split(a)
    z_0_0_0 = res[0]
    z_0_0_0_sender.send(z_0_0_0)
    y_0_0_0 = res[1]
    y_0_0_0_sender.send(y_0_0_0)
    x_0_0_0 = res[2]
    x_0_0_0_sender.send(x_0_0_0)
def main(a_1:str ):
    global a
    a, = a_1,
    c_0_0_sender, c_0_0_receiver = mp.Pipe()
    z_0_0_0_sender, z_0_0_0_receiver = mp.Pipe()
    x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
    second_0_0_0_sender, second_0_0_0_receiver = mp.Pipe()
    first_0_0_0_sender, first_0_0_0_receiver = mp.Pipe()
    tasks = [task_1, task_2, task_3, task_4]
    channels = [[c_0_0_sender, first_0_0_0_receiver, second_0_0_0_receiver], [second_0_0_0_sender, x_0_0_0_receiver], [first_0_0_0_sender, z_0_0_0_receiver], [z_0_0_0_sender, y_0_0_0_sender, x_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = c_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

destruct_3_use_in_loop_out :: ModuleSpan
destruct_3_use_in_loop_out = [pythonModule|
import multiprocessing as mp
from testLib import *
def task_1(third_0_0_0_sender, y_0_0_0_receiver, ctrl_0_1_receiver):
    while True:
        renew = False
        y_0_0_0_0 = y_0_0_0_receiver.recv()
        while not renew:
            sig = ctrl_0_1_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                third_0_0_0 = f(y_0_0_0_0)
                third_0_0_0_sender.send(third_0_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_2(ctrl_0_0_sender, ctrl_0_1_sender, z_0_0_0_receiver):
    while True:
        data = z_0_0_0_receiver.recv()
        hasSize = True if hasattr(data, '__len__') else False
        if hasSize:
            size = len(data)
            ctrl = True, size
            ctrl_0_0_sender.send(ctrl)
            ctrl = True, size
            ctrl_0_1_sender.send(ctrl)
        else:
            size = 0
            for d in data:
                ctrl = False, 1
                ctrl_0_0_sender.send(ctrl)
                ctrl = False, 1
                ctrl_0_1_sender.send(ctrl)
                size = size + 1
            ctrl = True, 0
            ctrl_0_0_sender.send(ctrl)
            ctrl = True, 0
            ctrl_0_1_sender.send(ctrl)
def task_3(z_0_0_0_sender, y_0_0_0_sender, x_0_0_1_sender, first_0_0_0_receiver):
    while True:
        var_0 = first_0_0_0_receiver.recv()
        res = destruct(var_0)
        z_0_0_0 = res[0]
        z_0_0_0_sender.send(z_0_0_0)
        y_0_0_0 = res[1]
        y_0_0_0_sender.send(y_0_0_0)
        x_0_0_1 = res[2]
        x_0_0_1_sender.send(x_0_0_1)
def task_4(first_0_0_0_sender):
    first_0_0_0 = split(a)
    first_0_0_0_sender.send(first_0_0_0)
def task_5(x_0_1_0_sender, x_0_0_1_receiver, ctrl_0_0_receiver, third_0_0_0_receiver):
    while True:
        renew = False
        x_0_0_1_0 = x_0_0_1_receiver.recv()
        while not renew:
            sig = ctrl_0_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                var_1 = third_0_0_0_receiver.recv()
                x_0_0_1_0.do_stuff(var_1)
            renew_next_time = sig[0]
            renew = renew_next_time
        x_0_1_0_sender.send(x_0_0_1_0)
def main(a_1:str ):
    global a
    a, = a_1,
    x_0_1_0_sender, x_0_1_0_receiver = mp.Pipe()
    first_0_0_0_sender, first_0_0_0_receiver = mp.Pipe()
    z_0_0_0_sender, z_0_0_0_receiver = mp.Pipe()
    x_0_0_1_sender, x_0_0_1_receiver = mp.Pipe()
    ctrl_0_0_sender, ctrl_0_0_receiver = mp.Pipe()
    y_0_0_0_sender, y_0_0_0_receiver = mp.Pipe()
    ctrl_0_1_sender, ctrl_0_1_receiver = mp.Pipe()
    third_0_0_0_sender, third_0_0_0_receiver = mp.Pipe()
    tasks = [task_1, task_2, task_3, task_4, task_5]
    channels = [[third_0_0_0_sender, y_0_0_0_receiver, ctrl_0_1_receiver], [ctrl_0_0_sender, ctrl_0_1_sender, z_0_0_0_receiver], [z_0_0_0_sender, y_0_0_0_sender, x_0_0_1_sender, first_0_0_0_receiver], [first_0_0_0_sender], [x_0_1_0_sender, x_0_0_1_receiver, ctrl_0_0_receiver, third_0_0_0_receiver]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_1_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]