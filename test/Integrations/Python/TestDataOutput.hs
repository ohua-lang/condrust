{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.TestDataOutput where

import Integrations.Python.SimpleQuoter


-- Test cases for Basic.hs
-- Test cases for Basic.hs
callAFunction = [pythonModule|
import multiprocessing as mp
a_0_sender, a_0_receiver = mp.Pipe()
x_0_0_sender, x_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0 = hello_world()
    x_0_0_sender.send(x_0_0)
def task_2():
    x_0_0_receiver.recv()
    x = None
    a_0_sender.send(x)
from testLib import *
def main():
    tasks = [task_1, task_2]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = a_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignNumLit = [pythonModule|
from testLib import *

def algo():
    x = 5
|]

assignNumLitReturn = [pythonModule|
from testLib import *

def algo():
    x = 5
    return x
|]

assignBinOp = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_0 = 42 + 23
    x_0_0_0_sender.send(x_0_0_0)

from testLib import *
def main():
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]


assignBools = [pythonModule|
import multiprocessing as mp
c_0_0_0_sender, c_0_0_0_receiver = mp.Pipe()
def task_1():
    c_0_0_0 = True and False
    c_0_0_0_sender.send(c_0_0_0)

from testLib import *
def main():
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = c_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignBinOpChained = [pythonModule|
import multiprocessing as mp
y_0_0_0_sender, y_0_0_0_receiver = mp.Pipe()
z_0_0_0_sender, z_0_0_0_receiver = mp.Pipe()
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_0 = f()
    x_0_0_0_sender.send(x_0_0_0)
    
def task_2():
    while True:
        var_0 = x_0_0_0_receiver.recv()
        var_1 = z_0_0_0_receiver.recv()
        y_0_0_0 = var_0 + var_1
        y_0_0_0_sender.send(y_0_0_0)
        
def task_3():
    z_0_0_0 = g()
    z_0_0_0_sender.send(z_0_0_0)
    
from testLib import *
def main():
    tasks = [task_1, task_2, task_3]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = y_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignAugmented = [pythonModule|
from testLib import *

def algo():
    x += 42
|]

assignSet  = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_0 = {a, b, 1, 2, 3}
    x_0_0_0_sender.send(x_0_0_0)
from testLib import *
def main(a_1, b_1):
    global a, b
    a, b = a_1, b_1
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

noReturn = [pythonModule|
import multiprocessing as mp
a_0_sender, a_0_receiver = mp.Pipe()
x_0_0_sender, x_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_receiver.recv()
    x = None
    a_0_sender.send(x)
def task_2():
    x_0_0 = funInt()
    x_0_0_sender.send(x_0_0)
from testLib import *
def main():
    tasks = [task_1, task_2]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = a_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

multiAssignment = [pythonModule|
from testLib import *

def algo():
    x = f()
    y = g()
    z = h()
|]

emptyReturn = [pythonModule|
import multiprocessing as mp
a_0_sender, a_0_receiver = mp.Pipe()
x_0_0_sender, x_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_receiver.recv()
    x = None
    a_0_sender.send(x)
def task_2():
    x_0_0 = funInt()
    x_0_0_sender.send(x_0_0)
from testLib import *
def main():
    tasks = [task_1, task_2]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = a_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

noneReturn = [pythonModule|
import multiprocessing as mp
a_0_sender, a_0_receiver = mp.Pipe()
x_0_0_sender, x_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_receiver.recv()
    x = None
    a_0_sender.send(x)
def task_2():
    x_0_0 = funInt()
    x_0_0_sender.send(x_0_0)
from testLib import *
def main():
    tasks = [task_1, task_2]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = a_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

exprNoReturn = [pythonModule|
import multiprocessing as mp
a_0_sender, a_0_receiver = mp.Pipe()
x_0_0_sender, x_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_receiver.recv()
    x = None
    a_0_sender.send(x)
def task_2():
    x_0_0 = funInt()
    x_0_0_sender.send(x_0_0)
from testLib import *
def main():
    tasks = [task_1, task_2]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = a_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|] 

varReturn = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_0 = oneArg(7)
    x_0_0_0_sender.send(x_0_0_0)
    
from testLib import *
def main():
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

otherVarReturn = [pythonModule|
from testLib import *

def algo():
    a = f(42)
    x = f()
    z = g()
    return x
|]

onlyReturnFunCall  = [pythonModule|
import multiprocessing as mp
a_0_0_sender, a_0_0_receiver = mp.Pipe()
def task_1():
    a_0_0 = f()
    a_0_0_sender.send(a_0_0)
    
from testLib import *
def main():
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = a_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]


chainedAssignment = [pythonModule|
import multiprocessing as mp
z_0_0_0_sender, z_0_0_0_receiver = mp.Pipe()
a_0_0_0_sender, a_0_0_0_receiver = mp.Pipe()
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = x_0_0_0_receiver.recv()
        z_0_0_0 = g(var_0)
        z_0_0_0_sender.send(z_0_0_0)
        
def task_2():
    while True:
        var_0 = a_0_0_0_receiver.recv()
        x_0_0_0 = f(var_0)
        x_0_0_0_sender.send(x_0_0_0)
        
def task_3():
    a_0_0_0 = f(42)
    a_0_0_0_sender.send(a_0_0_0)
    
from testLib import *
def main():
    tasks = [task_1, task_2, task_3]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = z_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignmentCallReturn = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
a_0_0_0_sender, a_0_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = a_0_0_0_receiver.recv()
        x_0_0_0 = f(var_0)
        x_0_0_0_sender.send(x_0_0_0)
        
def task_2():
    a_0_0_0 = f(42)
    a_0_0_0_sender.send(a_0_0_0)
    
from testLib import *
def main():
    tasks = [task_1, task_2]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignCallAssignReturn = [pythonModule|
import multiprocessing as mp
z_0_0_0_sender, z_0_0_0_receiver = mp.Pipe()
a_0_0_0_sender, a_0_0_0_receiver = mp.Pipe()
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = x_0_0_0_receiver.recv()
        z_0_0_0 = f(var_0)
        z_0_0_0_sender.send(z_0_0_0)
        
def task_2():
    while True:
        var_0 = a_0_0_0_receiver.recv()
        x_0_0_0 = f(var_0)
        x_0_0_0_sender.send(x_0_0_0)
        
def task_3():
    a_0_0_0 = f(42)
    a_0_0_0_sender.send(a_0_0_0)
    
from testLib import *
def main():
    tasks = [task_1, task_2, task_3]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = z_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]


nestedCompose = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
b_0_0_sender, b_0_0_receiver = mp.Pipe()
a_0_0_sender, a_0_0_receiver = mp.Pipe()
c_0_0_sender, c_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = c_0_0_receiver.recv()
        var_1 = a_0_0_receiver.recv()
        x_0_0_0 = moreArgs(var_0, var_1, 42)
        x_0_0_0_sender.send(x_0_0_0)
        
def task_2():
    b_0_0 = funInt()
    b_0_0_sender.send(b_0_0)
    
def task_3():
    while True:
        var_0 = b_0_0_receiver.recv()
        a_0_0 = oneArg(var_0)
        a_0_0_sender.send(a_0_0)
        
def task_4():
    c_0_0 = funInt()
    c_0_0_sender.send(c_0_0)
    
from testLib import *
def main():
    tasks = [task_1, task_2, task_3, task_4]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

tupleArgumentCall= [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
tpl_0_0_0_sender, tpl_0_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = tpl_0_0_0_receiver.recv()
        x_0_0_0 = oneArg(var_0)
        x_0_0_0_sender.send(x_0_0_0)
def task_2():
    tpl_0_0_0 = a, b
    tpl_0_0_0_sender.send(tpl_0_0_0)
from testLib import *
def main(a_1, b_1):
    global a, b
    a, b = a_1, b_1
    tasks = [task_1, task_2]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

algoWithParams = [pythonModule|
import multiprocessing as mp
c_0_0_sender, c_0_0_receiver = mp.Pipe()
y_0_0_0_sender, y_0_0_0_receiver = mp.Pipe()
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    y_0_0_0 = g1(b)
    y_0_0_0_sender.send(y_0_0_0)
    
def task_2():
    while True:
        var_0 = x_0_0_0_receiver.recv()
        var_1 = y_0_0_0_receiver.recv()
        c_0_0 = var_0 + var_1
        c_0_0_sender.send(c_0_0)
        
def task_3():
    x_0_0_0 = f(a)
    x_0_0_0_sender.send(x_0_0_0)
    
from testLib import *
def main(a_1, b_1):
    global a, b
    a, b = a_1, b_1
    tasks = [task_1, task_2, task_3]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = c_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

applyLambdaExpr = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_0 = 2 * 3
    x_0_0_0_sender.send(x_0_0_0)
    
from testLib import *
def main(a_1, b_1):
    global a, b
    a, b = a_1, b_1
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignLambdaExpr = [pythonModule|
import multiprocessing as mp
y_0_0_0_sender, y_0_0_0_receiver = mp.Pipe()
def task_1():
    y_0_0_0 = 2 * 3
    y_0_0_0_sender.send(y_0_0_0)
    
from testLib import *
def main(a_1, b_1):
    global a, b
    a, b = a_1, b_1
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = y_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignEmptyList = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_0 = []
    x_0_0_0_sender.send(x_0_0_0)
from testLib import *
def main(a_1, b_1):
    global a, b
    a, b = a_1, b_1
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignList = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_0 = [a, b]
    x_0_0_0_sender.send(x_0_0_0)
from testLib import *
def main(a_1, b_1):
    global a, b
    a, b = a_1, b_1
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignEmptyDict = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_0 = {}
    x_0_0_0_sender.send(x_0_0_0)
from testLib import *
def main(a_1, b_1):
    global a, b
    a, b = a_1, b_1
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignDict = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
c_0_0_sender, c_0_0_receiver = mp.Pipe()
d_0_0_sender, d_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = d_0_0_receiver.recv()
        var_1 = c_0_0_receiver.recv()
        x_0_0_0 = {var_0[0]: var_0[1], var_1[0]: var_1[1]}
        x_0_0_0_sender.send(x_0_0_0)
def task_2():
    d_0_0 = a, 1
    d_0_0_sender.send(d_0_0)
def task_3():
    c_0_0 = b, 2
    c_0_0_sender.send(c_0_0)
from testLib import *
def main(a_1, b_1):
    global a, b
    a, b = a_1, b_1
    tasks = [task_1, task_2, task_3]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]



--Test cases for State.hs ---------------------------------------------
callMethod = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
mob_0_0_1_sender, mob_0_0_1_receiver = mp.Pipe()
def task_1():
    mob_0_0_1 = MObs(22)
    mob_0_0_1_sender.send(mob_0_0_1)
    
def task_2():
    while True:
        var_0 = mob_0_0_1_receiver.recv()
        x_0_0_0 = var_0.getNum()
        x_0_0_0_sender.send(x_0_0_0)
        
from testLib import *
def main():
    tasks = [task_1, task_2]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

--Test cases for Loops.hs ---------------------------------------------
loopIterator= [pythonModule|
import multiprocessing as mp
mOb_0_1_0_sender, mOb_0_1_0_receiver = mp.Pipe()
mOb_0_0_1_sender, mOb_0_0_1_receiver = mp.Pipe()
ctrl_0_0_sender, ctrl_0_0_receiver = mp.Pipe()
d_0_0_sender, d_0_0_receiver = mp.Pipe()
n_0_0_0_sender, n_0_0_0_receiver = mp.Pipe()
def task_1():
    mOb_0_0_1 = MObs(42)
    mOb_0_0_1_sender.send(mOb_0_0_1)
def task_2():
    while True:
        var_0 = d_0_0_receiver.recv()
        n_0_0_0 = f(var_0)
        n_0_0_0_sender.send(n_0_0_0)
def task_3():
    g_0_0_0 = some_invented_iter_function()
    while True:
        data = g_0_0_0
        hasSize = True if hasattr(data, '__len__') else False
        if hasSize:
            size = len(data)
            ctrl = True, size
            ctrl_0_0_sender.send(ctrl)
            for d in data:
                d_0_0_sender.send(d)
        else:
            size = 0
            for d in data:
                d_0_0_sender.send(d)
                ctrl = False, 1
                ctrl_0_0_sender.send(ctrl)
                size = size + 1
            ctrl = True, 0
            ctrl_0_0_sender.send(ctrl)
def task_4():
    while True:
        renew = False
        mOb_0_0_1_0 = mOb_0_0_1_receiver.recv()
        while not renew:
            sig = ctrl_0_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                var_1 = n_0_0_0_receiver.recv()
                mOb_0_0_1_0.addNum(var_1)
            renew_next_time = sig[0]
            renew = renew_next_time
        mOb_0_1_0_sender.send(mOb_0_0_1_0)
from testLib import *
def main(a_1):
    global a
    a, = a_1,
    tasks = [task_1, task_2, task_3, task_4]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = mOb_0_1_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]


loopIterObj= [pythonModule|
import multiprocessing as mp
mOb_0_1_0_sender, mOb_0_1_0_receiver = mp.Pipe()
g_0_0_1_sender, g_0_0_1_receiver = mp.Pipe()
mOb_0_0_1_sender, mOb_0_0_1_receiver = mp.Pipe()
ctrl_0_0_sender, ctrl_0_0_receiver = mp.Pipe()
d_1_0_sender, d_1_0_receiver = mp.Pipe()
n_0_0_0_sender, n_0_0_0_receiver = mp.Pipe()
def task_1():
    g_0_0_1 = {}
    g_0_0_1_sender.send(g_0_0_1)
def task_2():
    while True:
        var_0 = d_1_0_receiver.recv()
        n_0_0_0 = f(var_0)
        n_0_0_0_sender.send(n_0_0_0)
def task_3():
    mOb_0_0_1 = MObs(42)
    mOb_0_0_1_sender.send(mOb_0_0_1)
def task_4():
    var_0 = g_0_0_1_receiver.recv()
    a_1_0 = var_0.values()
    None
    data = a_1_0
    hasSize = True if hasattr(data, '__len__') else False
    if hasSize:
        size = len(data)
        ctrl = True, size
        ctrl_0_0_sender.send(ctrl)
        for d in data:
            d_1_0_sender.send(d)
    else:
        size = 0
        for d in data:
            d_1_0_sender.send(d)
            ctrl = False, 1
            ctrl_0_0_sender.send(ctrl)
            size = size + 1
        ctrl = True, 0
        ctrl_0_0_sender.send(ctrl)
def task_5():
    while True:
        renew = False
        mOb_0_0_1_0 = mOb_0_0_1_receiver.recv()
        while not renew:
            sig = ctrl_0_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                var_1 = n_0_0_0_receiver.recv()
                mOb_0_0_1_0.addNum(var_1)
            renew_next_time = sig[0]
            renew = renew_next_time
        mOb_0_1_0_sender.send(mOb_0_0_1_0)
from testLib import *
def main(a_1):
    global a
    a, = a_1,
    tasks = [task_1, task_2, task_3, task_4, task_5]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = mOb_0_1_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]


loopTuplePattern= [pythonModule|
from testLib import *
# Todo
|]

loopCommaPattern= [pythonModule|
from testLib import *
# Todo
|]

whileLoop = [pythonModule|
from testLib import *
# ToDo
|]
--Test cases for IfElse.hs --------------------------------------------
iteLiteralArgs = [pythonModule|
from testLib import *
# ToDO
|]

branchReturn = [pythonModule|
from testLib import *
# ToDo
|] 

iteStateful = [pythonModule|
from testLib import *
# ToDo
|] 

iteRustExample = [pythonModule|
from testLib import *
# TODO
|]


condExprLit = [pythonModule|
from testLib import *

def algo(i):
    c = f2(i)
    b = id(i)
    x = f(b) if c else g(42)
    return x
|]

condExpr = [pythonModule|
from testLib import *
# ToDo 
|]


condExpr2 = [pythonModule|
from testLib import *
# ToDo
|]

--Test cases for TailRec.hs --------------------------------------------
tailRec= [pythonModule|
from testLib import *

# TODO
|]

tailRecMultiArg= [pythonModule|
from testLib import *
# TODO
|]

tailRecContext= [pythonModule|
from testLib import *

# TODO
|]

---------------------------

twoAlgos = [pythonModule|
from testLib import *

def algo1():
    mob = MObs(22)
    # mob.addNum(21)
    x = mob.getNum()
    return x

def algo2():
    x = 5
    y = algo1(x) 
    return x
|]
