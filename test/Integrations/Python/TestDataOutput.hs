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
    while True:
        var_0 = x_0_0_0_receiver.recv()
        var_1 = z_0_0_0_receiver.recv()
        y_0_0_0 = var_0 + var_1
        y_0_0_0_sender.send(y_0_0_0)
def task_2():
    x_0_0_0 = f()
    x_0_0_0_sender.send(x_0_0_0)
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
    a_0_0_0 = f(42)
    a_0_0_0_sender.send(a_0_0_0)
def task_3():
    while True:
        var_0 = a_0_0_0_receiver.recv()
        x_0_0_0 = f(var_0)
        x_0_0_0_sender.send(x_0_0_0)
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
    a_0_0_0 = f(42)
    a_0_0_0_sender.send(a_0_0_0)
def task_3():
    while True:
        var_0 = a_0_0_0_receiver.recv()
        x_0_0_0 = f(var_0)
        x_0_0_0_sender.send(x_0_0_0)
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
    while True:
        var_0 = x_0_0_0_receiver.recv()
        var_1 = y_0_0_0_receiver.recv()
        c_0_0 = var_0 + var_1
        c_0_0_sender.send(c_0_0)
def task_2():
    y_0_0_0 = g1(b)
    y_0_0_0_sender.send(y_0_0_0)
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
    c_0_0 = b, 2
    c_0_0_sender.send(c_0_0)
def task_3():
    d_0_0 = a, 1
    d_0_0_sender.send(d_0_0)
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
    while True:
        var_0 = mob_0_0_1_receiver.recv()
        x_0_0_0 = var_0.getNum()
        x_0_0_0_sender.send(x_0_0_0)
def task_2():
    mob_0_0_1 = MObs(22)
    mob_0_0_1_sender.send(mob_0_0_1)
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

flat= [pythonModule|
import multiprocessing as mp
a_0_0_sender, a_0_0_receiver = mp.Pipe()
mob_0_0_1_sender, mob_0_0_1_receiver = mp.Pipe()
result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
def task_1():
    mob_0_0_1 = MObs(i)
    mob_0_0_1_sender.send(mob_0_0_1)
def task_2():
    while True:
        var_0 = result_0_0_0_receiver.recv()
        a_0_0 = oneArg(var_0)
        a_0_0_sender.send(a_0_0)
def task_3():
    while True:
        var_0 = mob_0_0_1_receiver.recv()
        result_0_0_0 = var_0.getNum()
        result_0_0_0_sender.send(result_0_0_0)
from testLib import *
def main(i_1):
    global i
    i, = i_1,
    tasks = [task_1, task_2, task_3]
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

thread = [pythonModule|
import multiprocessing as mp
result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
mob_0_0_2_sender, mob_0_0_2_receiver = mp.Pipe()
mob_0_0_1_0_sender, mob_0_0_1_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = mob_0_0_1_0_receiver.recv()
        result_0_0_0 = var_0.getNum()
        result_0_0_0_sender.send(result_0_0_0)
def task_2():
    while True:
        var_0 = mob_0_0_2_receiver.recv()
        var_0.addNum(23)
        mob_0_0_1_0_sender.send(var_0)
def task_3():
    mob_0_0_2 = MObs(i)
    mob_0_0_2_sender.send(mob_0_0_2)
from testLib import *
def main(i_1):
    global i
    i, = i_1,
    tasks = [task_1, task_2, task_3]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

loop = [pythonModule|
from testLib import *

def algo(a):
    g = listOfMObs(a)
    for e in g:
        e.addNum(7)
|]

singleIO = [pythonModule|
from testLib import *

def algo(a):
    mob = MObs(a)
    g = some_invented_iter_function()
    for e in g:
        mob.addNum(e)
|]

singleState = [pythonModule|
import multiprocessing as mp
c_0_0_sender, c_0_0_receiver = mp.Pipe()
mob_0_0_1_sender, mob_0_0_1_receiver = mp.Pipe()
ctrl_0_0_sender, ctrl_0_0_receiver = mp.Pipe()
d_1_sender, d_1_receiver = mp.Pipe()
mob_0_1_1_sender, mob_0_1_1_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = mob_0_1_1_receiver.recv()
        c_0_0 = var_0.getNum()
        c_0_0_sender.send(c_0_0)
def task_2():
    mob_0_0_1 = MObs(a)
    mob_0_0_1_sender.send(mob_0_0_1)
def task_3():
    g_0_0_0 = some_invented_iter_function()
    while True:
        hasSize = True if hasattr(g_0_0_0, '__len__') else False
        if hasSize:
            size = len(g_0_0_0)
            ctrl = True, size
            ctrl_0_0_sender.send(ctrl)
            for d in g_0_0_0:
                d_1_sender.send(d)
        else:
            size = 0
            for d in g_0_0_0:
                d_1_sender.send(d)
                ctrl = False, 1
                ctrl_0_0_sender.send(ctrl)
                size = size + 1
            ctrl = True, 0
            ctrl_0_0_sender.send(ctrl)
def task_4():
    while True:
        renew = False
        mob_0_0_1_0 = mob_0_0_1_receiver.recv()
        while not renew:
            sig = ctrl_0_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                var_1 = d_1_receiver.recv()
                mob_0_0_1_0.addNum(var_1)
            renew_next_time = sig[0]
            renew = renew_next_time
        mob_0_1_1_sender.send(mob_0_0_1_0)
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
    result = c_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

stateOut = [pythonModule|
import multiprocessing as mp
mob_0_1_0_sender, mob_0_1_0_receiver = mp.Pipe()
mob_0_0_2_sender, mob_0_0_2_receiver = mp.Pipe()
mob2_0_0_0_sender, mob2_0_0_0_receiver = mp.Pipe()
ctrl_0_0_sender, ctrl_0_0_receiver = mp.Pipe()
mob_0_0_1_0_sender, mob_0_0_1_0_receiver = mp.Pipe()
ctrl_0_1_sender, ctrl_0_1_receiver = mp.Pipe()
d_1_sender, d_1_receiver = mp.Pipe()
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    mob_0_0_2 = MObs(a)
    mob_0_0_2_sender.send(mob_0_0_2)
def task_2():
    while True:
        renew = False
        mob2_0_0_0_0 = mob2_0_0_0_receiver.recv()
        while not renew:
            sig = ctrl_0_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                var_1 = d_1_receiver.recv()
                x_0_0_0 = mobFun(mob2_0_0_0_0, var_1)
                x_0_0_0_sender.send(x_0_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_3():
    while True:
        var_0 = mob_0_0_2_receiver.recv()
        mob2_0_0_0 = var_0.clone()
        mob2_0_0_0_sender.send(mob2_0_0_0)
        mob_0_0_1_0_sender.send(var_0)
def task_4():
    g_0_0_0 = some_invented_iter_function()
    while True:
        hasSize = True if hasattr(g_0_0_0, '__len__') else False
        if hasSize:
            size = len(g_0_0_0)
            ctrl = True, size
            ctrl_0_0_sender.send(ctrl)
            ctrl = True, size
            ctrl_0_1_sender.send(ctrl)
            for d in g_0_0_0:
                d_1_sender.send(d)
        else:
            size = 0
            for d in g_0_0_0:
                d_1_sender.send(d)
                ctrl = False, 1
                ctrl_0_0_sender.send(ctrl)
                ctrl = False, 1
                ctrl_0_1_sender.send(ctrl)
                size = size + 1
            ctrl = True, 0
            ctrl_0_0_sender.send(ctrl)
            ctrl = True, 0
            ctrl_0_1_sender.send(ctrl)
def task_5():
    while True:
        renew = False
        mob_0_0_1_0_0 = mob_0_0_1_0_receiver.recv()
        while not renew:
            sig = ctrl_0_1_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                var_1 = x_0_0_0_receiver.recv()
                mob_0_0_1_0_0.addNum(var_1)
            renew_next_time = sig[0]
            renew = renew_next_time
        mob_0_1_0_sender.send(mob_0_0_1_0_0)
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
    result = mob_0_1_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]



--Test cases for Loops.hs ---------------------------------------------
loopIterator= [pythonModule|
import multiprocessing as mp
s_0_1_0_sender, s_0_1_0_receiver = mp.Pipe()
s_0_0_1_sender, s_0_0_1_receiver = mp.Pipe()
ctrl_0_0_sender, ctrl_0_0_receiver = mp.Pipe()
d_0_sender, d_0_receiver = mp.Pipe()
r_0_0_0_sender, r_0_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = d_0_receiver.recv()
        r_0_0_0 = h(var_0)
        r_0_0_0_sender.send(r_0_0_0)
def task_2():
    s_0_0_1 = MObs(42)
    s_0_0_1_sender.send(s_0_0_1)
def task_3():
    stream_0_0_0 = some_invented_iter_function()
    while True:
        hasSize = True if hasattr(stream_0_0_0, '__len__') else False
        if hasSize:
            size = len(stream_0_0_0)
            ctrl = True, size
            ctrl_0_0_sender.send(ctrl)
            for d in stream_0_0_0:
                d_0_sender.send(d)
        else:
            size = 0
            for d in stream_0_0_0:
                d_0_sender.send(d)
                ctrl = False, 1
                ctrl_0_0_sender.send(ctrl)
                size = size + 1
            ctrl = True, 0
            ctrl_0_0_sender.send(ctrl)
def task_4():
    while True:
        renew = False
        s_0_0_1_0 = s_0_0_1_receiver.recv()
        while not renew:
            sig = ctrl_0_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                var_1 = r_0_0_0_receiver.recv()
                s_0_0_1_0.addNum(var_1)
            renew_next_time = sig[0]
            renew = renew_next_time
        s_0_1_0_sender.send(s_0_0_1_0)
from testLib import *
def main():
    tasks = [task_1, task_2, task_3, task_4]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = s_0_1_0_receiver.recv()
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
d_1_sender, d_1_receiver = mp.Pipe()
n_0_0_0_sender, n_0_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = d_1_receiver.recv()
        n_0_0_0 = f(var_0)
        n_0_0_0_sender.send(n_0_0_0)
def task_2():
    g_0_0_1 = {}
    g_0_0_1_sender.send(g_0_0_1)
def task_3():
    var_0 = g_0_0_1_receiver.recv()
    a_1_0 = var_0.values()
    None
    hasSize = True if hasattr(a_1_0, '__len__') else False
    if hasSize:
        size = len(a_1_0)
        ctrl = True, size
        ctrl_0_0_sender.send(ctrl)
        for d in a_1_0:
            d_1_sender.send(d)
    else:
        size = 0
        for d in a_1_0:
            d_1_sender.send(d)
            ctrl = False, 1
            ctrl_0_0_sender.send(ctrl)
            size = size + 1
        ctrl = True, 0
        ctrl_0_0_sender.send(ctrl)
def task_4():
    mOb_0_0_1 = MObs(42)
    mOb_0_0_1_sender.send(mOb_0_0_1)
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
import multiprocessing as mp
g_0_0_sender, g_0_0_receiver = mp.Pipe()
a_0_0_0_0_sender, a_0_0_0_0_receiver = mp.Pipe()
b_0_0_0_sender, b_0_0_0_receiver = mp.Pipe()
ctrlTrue_0_sender, ctrlTrue_0_receiver = mp.Pipe()
c_0_0_0_sender, c_0_0_0_receiver = mp.Pipe()
ctrlFalse_0_sender, ctrlFalse_0_receiver = mp.Pipe()
f_0_0_sender, f_0_0_receiver = mp.Pipe()
e_0_0_sender, e_0_0_receiver = mp.Pipe()
a_0_0_0_1_sender, a_0_0_0_1_receiver = mp.Pipe()
result_0_sender, result_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = result_0_receiver.recv()
        g_0_0 = oneArg(var_0)
        g_0_0_sender.send(g_0_0)
def task_2():
    c_0_0_0 = f2(3)
    c_0_0_0_sender.send(c_0_0_0)
def task_3():
    while True:
        branchSelection = a_0_0_0_1_receiver.recv()
        if branchSelection:
            result = e_0_0_receiver.recv()
            result_0_sender.send(result)
        else:
            result = f_0_0_receiver.recv()
            result_0_sender.send(result)
def task_4():
    while True:
        branchSelection = a_0_0_0_0_receiver.recv()
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
def task_5():
    res = f()
    a_0_0_0_0_sender.send(res)
    a_0_0_0_1_sender.send(res)
def task_6():
    b_0_0_0 = f1(2)
    b_0_0_0_sender.send(b_0_0_0)
def task_7():
    while True:
        renew = False
        c_0_0_0_0 = c_0_0_0_receiver.recv()
        while not renew:
            sig = ctrlFalse_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                f_0_0 = g1(c_0_0_0_0)
                f_0_0_sender.send(f_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_8():
    while True:
        renew = False
        b_0_0_0_0 = b_0_0_0_receiver.recv()
        while not renew:
            sig = ctrlTrue_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                e_0_0 = g0(b_0_0_0_0)
                e_0_0_sender.send(e_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
from testLib import *
def main(i_1):
    global i
    i, = i_1,
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = g_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]



condExprState = [pythonModule|
import multiprocessing as mp
h_0_0_sender, h_0_0_receiver = mp.Pipe()
mob_0_0_1_sender, mob_0_0_1_receiver = mp.Pipe()
e_0_0_0_sender, e_0_0_0_receiver = mp.Pipe()
b_0_0_0_sender, b_0_0_0_receiver = mp.Pipe()
ctrlTrue_0_sender, ctrlTrue_0_receiver = mp.Pipe()
c_0_0_0_sender, c_0_0_0_receiver = mp.Pipe()
ctrlFalse_0_sender, ctrlFalse_0_receiver = mp.Pipe()
g_0_0_sender, g_0_0_receiver = mp.Pipe()
f_0_0_sender, f_0_0_receiver = mp.Pipe()
e_0_0_1_sender, e_0_0_1_receiver = mp.Pipe()
result_0_sender, result_0_receiver = mp.Pipe()
def task_1():
    while True:
        renew = False
        c_0_0_0_0 = c_0_0_0_receiver.recv()
        while not renew:
            sig = ctrlFalse_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                g_0_0 = g1(c_0_0_0_0)
                g_0_0_sender.send(g_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_2():
    mob_0_0_1 = MObs(3)
    mob_0_0_1_sender.send(mob_0_0_1)
def task_3():
    while True:
        branchSelection = e_0_0_1_receiver.recv()
        if branchSelection:
            result = f_0_0_receiver.recv()
            result_0_sender.send(result)
        else:
            result = g_0_0_receiver.recv()
            result_0_sender.send(result)
def task_4():
    while True:
        var_0 = result_0_receiver.recv()
        h_0_0 = oneArg(var_0)
        h_0_0_sender.send(h_0_0)
def task_5():
    while True:
        renew = False
        b_0_0_0_0 = b_0_0_0_receiver.recv()
        while not renew:
            sig = ctrlTrue_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                f_0_0 = g0(b_0_0_0_0)
                f_0_0_sender.send(f_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_6():
    while True:
        branchSelection = e_0_0_0_receiver.recv()
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
def task_7():
    res = double(i)
    b_0_0_0 = res[0]
    b_0_0_0_sender.send(b_0_0_0)
    c_0_0_0 = res[1]
    c_0_0_0_sender.send(c_0_0_0)
def task_8():
    while True:
        var_0 = mob_0_0_1_receiver.recv()
        res = var_0.getNum()
        e_0_0_0_sender.send(res)
        e_0_0_1_sender.send(res)
from testLib import *
def main(i_1):
    global i
    i, = i_1,
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = h_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]


condContextFunction = [pythonModule|
import multiprocessing as mp
e_0_0_sender, e_0_0_receiver = mp.Pipe()
a_0_0_0_0_sender, a_0_0_0_0_receiver = mp.Pipe()
ctrlTrue_0_sender, ctrlTrue_0_receiver = mp.Pipe()
ctrlFalse_0_sender, ctrlFalse_0_receiver = mp.Pipe()
c_0_0_sender, c_0_0_receiver = mp.Pipe()
b_0_0_sender, b_0_0_receiver = mp.Pipe()
a_0_0_0_1_sender, a_0_0_0_1_receiver = mp.Pipe()
result_0_sender, result_0_receiver = mp.Pipe()
def task_1():
    while True:
        renew = False
        while not renew:
            sig = ctrlTrue_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                b_0_0 = g0(5)
                b_0_0_sender.send(b_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_2():
    while True:
        var_0 = result_0_receiver.recv()
        e_0_0 = oneArg(var_0)
        e_0_0_sender.send(e_0_0)
def task_3():
    res = f1(i)
    a_0_0_0_0_sender.send(res)
    a_0_0_0_1_sender.send(res)
def task_4():
    while True:
        branchSelection = a_0_0_0_0_receiver.recv()
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
def task_5():
    while True:
        branchSelection = a_0_0_0_1_receiver.recv()
        if branchSelection:
            result = b_0_0_receiver.recv()
            result_0_sender.send(result)
        else:
            result = c_0_0_receiver.recv()
            result_0_sender.send(result)
def task_6():
    while True:
        renew = False
        while not renew:
            sig = ctrlFalse_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                c_0_0 = f()
                c_0_0_sender.send(c_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
from testLib import *
def main(i_1):
    global i
    i, = i_1,
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = e_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

condExprStateFunRet = [pythonModule|
import multiprocessing as mp
result_0_sender, result_0_receiver = mp.Pipe()
b_0_0_0_0_sender, b_0_0_0_0_receiver = mp.Pipe()
mob1_0_0_1_sender, mob1_0_0_1_receiver = mp.Pipe()
ctrlTrue_0_sender, ctrlTrue_0_receiver = mp.Pipe()
mob2_0_0_1_sender, mob2_0_0_1_receiver = mp.Pipe()
ctrlFalse_0_sender, ctrlFalse_0_receiver = mp.Pipe()
c_0_0_sender, c_0_0_receiver = mp.Pipe()
a_0_0_sender, a_0_0_receiver = mp.Pipe()
b_0_0_0_1_sender, b_0_0_0_1_receiver = mp.Pipe()
def task_1():
    while True:
        branchSelection = b_0_0_0_1_receiver.recv()
        if branchSelection:
            result = a_0_0_receiver.recv()
            result_0_sender.send(result)
        else:
            result = c_0_0_receiver.recv()
            result_0_sender.send(result)
def task_2():
    mob2_0_0_1 = MObs(23)
    mob2_0_0_1_sender.send(mob2_0_0_1)
def task_3():
    while True:
        renew = False
        mob1_0_0_1_0 = mob1_0_0_1_receiver.recv()
        while not renew:
            sig = ctrlTrue_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                a_0_0 = mob1_0_0_1_0.getNum()
                a_0_0_sender.send(a_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_4():
    while True:
        branchSelection = b_0_0_0_0_receiver.recv()
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
def task_5():
    while True:
        renew = False
        mob2_0_0_1_0 = mob2_0_0_1_receiver.recv()
        while not renew:
            sig = ctrlFalse_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                c_0_0 = mob2_0_0_1_0.getNum()
                c_0_0_sender.send(c_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_6():
    res = f()
    b_0_0_0_0_sender.send(res)
    b_0_0_0_1_sender.send(res)
def task_7():
    mob1_0_0_1 = MObs(3)
    mob1_0_0_1_sender.send(mob1_0_0_1)
from testLib import *
def main(i_1):
    global i
    i, = i_1,
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

condExprFunCond = [pythonModule|
import multiprocessing as mp
result_0_sender, result_0_receiver = mp.Pipe()
a_0_0_0_sender, a_0_0_0_receiver = mp.Pipe()
ctrlTrue_0_sender, ctrlTrue_0_receiver = mp.Pipe()
ctrlFalse_0_sender, ctrlFalse_0_receiver = mp.Pipe()
c_0_0_sender, c_0_0_receiver = mp.Pipe()
b_0_0_sender, b_0_0_receiver = mp.Pipe()
a_0_0_1_sender, a_0_0_1_receiver = mp.Pipe()
def task_1():
    while True:
        renew = False
        while not renew:
            sig = ctrlTrue_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                b_0_0 = g()
                b_0_0_sender.send(b_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_2():
    while True:
        branchSelection = a_0_0_1_receiver.recv()
        if branchSelection:
            result = b_0_0_receiver.recv()
            result_0_sender.send(result)
        else:
            result = c_0_0_receiver.recv()
            result_0_sender.send(result)
def task_3():
    while True:
        renew = False
        while not renew:
            sig = ctrlFalse_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                c_0_0 = f1(4)
                c_0_0_sender.send(c_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_4():
    res = f()
    a_0_0_0_sender.send(res)
    a_0_0_1_sender.send(res)
def task_5():
    while True:
        branchSelection = a_0_0_0_receiver.recv()
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
from testLib import *
def main(i_1):
    global i
    i, = i_1,
    tasks = [task_1, task_2, task_3, task_4, task_5]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
    |]


--Test cases for TailRec.hs --------------------------------------------
tailRec= [pythonModule|
import multiprocessing as mp
d_0_0_sender, d_0_0_receiver = mp.Pipe()
returnV_0_0_0_sender, returnV_0_0_0_receiver = mp.Pipe()
b_0_0_sender, b_0_0_receiver = mp.Pipe()
i_0_0_0_sender, i_0_0_0_receiver = mp.Pipe()
j_0_0_0_sender, j_0_0_0_receiver = mp.Pipe()
k_0_0_0_sender, k_0_0_0_receiver = mp.Pipe()
recursionFlag_0_0_0_sender, recursionFlag_0_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = i_0_0_0_receiver.recv()
        res = double(var_0)
        j_0_0_0 = res[0]
        j_0_0_0_sender.send(j_0_0_0)
        k_0_0_0 = res[1]
        k_0_0_0_sender.send(k_0_0_0)
def task_2():
    while True:
        var_0 = k_0_0_0_receiver.recv()
        returnV_0_0_0 = g0(var_0)
        returnV_0_0_0_sender.send(returnV_0_0_0)
def task_3():
    while True:
        var_0 = recursionFlag_0_0_0_receiver.recv()
        b_0_0 = check(var_0)
        b_0_0_sender.send(b_0_0)
def task_4():
    while True:
        var_0 = j_0_0_0_receiver.recv()
        recursionFlag_0_0_0 = g0(var_0)
        recursionFlag_0_0_0_sender.send(recursionFlag_0_0_0)
def task_5():
    i_0_0_0_sender.send(1)
    while b_0_0_receiver.recv():
        loop_res_0 = returnV_0_0_0_receiver.recv()
        i_0_0_0_sender.send(loop_res_0)
    finalResult = returnV_0_0_0_receiver.recv()
    d_0_0_sender.send(finalResult)
from testLib import *
def main():
    tasks = [task_1, task_2, task_3, task_4, task_5]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = d_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
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
