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
def algo():
    hello_world()
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

assignBinOp = [pythonModule|
import multiprocessing as mp
x_0_0_sender, x_0_0_receiver = mp.Pipe()
def task_1():
    var_0 = 42
    var_1 = 23
    x_0_0 = var_0 + var_1
    x_0_0_sender.send(x_0_0)
from testLib import *
def algo():
    x = 42 + 23
    return x
def main():
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]


assignBinOpChained = [pythonModule|
import multiprocessing as mp
y_0_0_sender, y_0_0_receiver = mp.Pipe()
z_0_0_sender, z_0_0_receiver = mp.Pipe()
x_0_0_sender, x_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = x_0_0_receiver.recv()
        var_1 = z_0_0_receiver.recv()
        y_0_0 = var_0 + var_1
        y_0_0_sender.send(y_0_0)
def task_2():
    x_0_0 = f()
    x_0_0_sender.send(x_0_0)
def task_3():
    z_0_0 = g()
    z_0_0_sender.send(z_0_0)
from testLib import *
def algo():
    x = f()
    z = g()
    y = x + z
    return y
def main():
    tasks = [task_1, task_2, task_3]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = y_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignAugmented = [pythonModule|
from testLib import *

def algo():
    x += 42
|]

noReturn = [pythonModule|
from testLib import *

def algo():
    x = f()
|]

multiAssignment = [pythonModule|
from testLib import *

def algo():
    x = f()
    y = g()
    z = h()
|]

emptyReturn = [pythonModule|
from testLib import *

def algo():
    x = f()
    return
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
def algo():
    x = funInt()
    return None
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
def algo():
    funInt()
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
x_0_0_sender, x_0_0_receiver = mp.Pipe()
def task_1():
    var_0 = 7
    x_0_0 = oneArg(var_0)
    x_0_0_sender.send(x_0_0)
from testLib import *
def algo():
    x = oneArg(7)
    return x
def main():
    tasks = [task_1]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_receiver.recv()
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
a_0_sender, a_0_receiver = mp.Pipe()
def task_1():
    a_0 = f()
    a_0_sender.send(a_0)
from testLib import *
def algo():
    return f()
def main():
    tasks = [task_1]
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


chainedAssignment = [pythonModule|
import multiprocessing as mp
z_0_0_sender, z_0_0_receiver = mp.Pipe()
a_0_0_sender, a_0_0_receiver = mp.Pipe()
x_0_0_sender, x_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = a_0_0_receiver.recv()
        x_0_0 = f(var_0)
        x_0_0_sender.send(x_0_0)
def task_2():
    var_0 = 42
    a_0_0 = f(var_0)
    a_0_0_sender.send(a_0_0)
def task_3():
    while True:
        var_0 = x_0_0_receiver.recv()
        z_0_0 = g(var_0)
        z_0_0_sender.send(z_0_0)
from testLib import *
def algo():
    a = f(42)
    x = f(a)
    z = g(x)
    return z
def main():
    tasks = [task_1, task_2, task_3]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = z_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

assignmentCallReturn = [pythonModule|
from testLib import *

def algo():
    a = f(42)
    x = f(a)
    g(x)
    return x
|]

assignCallAssignReturn = [pythonModule|
from testLib import *

def algo():
    a = f(42)
    x = f(a)
    g(x)
    z = f(x)
    return z
|]


nestedCompose = [pythonModule|
import multiprocessing as mp
x_0_0_sender, x_0_0_receiver = mp.Pipe()
b_0_sender, b_0_receiver = mp.Pipe()
a_0_sender, a_0_receiver = mp.Pipe()
c_0_sender, c_0_receiver = mp.Pipe()
def task_1():
    c_0 = funInt()
    c_0_sender.send(c_0)
def task_2():
    while True:
        var_0 = c_0_receiver.recv()
        var_1 = a_0_receiver.recv()
        var_2 = 42
        x_0_0 = moreArgs(var_0, var_1, var_2)
        x_0_0_sender.send(x_0_0)
def task_3():
    b_0 = funInt()
    b_0_sender.send(b_0)
def task_4():
    while True:
        var_0 = b_0_receiver.recv()
        a_0 = oneArg(var_0)
        a_0_sender.send(a_0)
from testLib import *
def algo():
    x = moreArgs(funInt(), oneArg(funInt()), 42)
    return x
def main():
    tasks = [task_1, task_2, task_3, task_4]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = x_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]


--Test cases for Loops.hs ---------------------------------------------
loopIterator= [pythonModule|
from testLib import *

def algo(a):
    g = some_invented_iter_function()
    for i in g:
        f(i)
|]

whileLoop = [pythonModule|
from testLib import *

def algo(a):
    i = something()
    while testfun(i):
        i = manipulate(i)
    return i
|]
--Test cases for IfElse.hs --------------------------------------------
ifThenAssign = [pythonModule|
from testLib import *

def algo(a):
    if a:
        x = g()
    else:
        x = f()
    return x
|]

branchReturn = [pythonModule|
from testLib import *

def algo(i):
    if a:
        d = g0(b)
    else:
        return
    return h(d) 
|] 

ifThenMultiVar = [pythonModule|
from testLib import *

def algo(i):
    a = f(i)
    b = f1(i)
    c = f2(i)
    if a:
        d = g0(b)
    else:
        d = g1(c)
    return h(d) 
|]

condExpr = [pythonModule|
from testLib import *

def algo(i):
    return None
    # c = someCheck(i)
    # x = f(i) if c else g(i)
|]


