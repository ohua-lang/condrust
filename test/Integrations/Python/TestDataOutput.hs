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
    None
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
    None
from testLib import *
def algo():
    a = True
    b = False
    c = a and b
    return c
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
    None
def task_2():
    while True:
        var_0 = x_0_0_0_receiver.recv()
        var_1 = z_0_0_0_receiver.recv()
        y_0_0_0 = var_0 + var_1
        y_0_0_0_sender.send(y_0_0_0)
        None
def task_3():
    z_0_0_0 = g()
    z_0_0_0_sender.send(z_0_0_0)
    None
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
def algo():
    x = funInt()
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
def algo():
    x = funInt()
    return
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
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_0 = oneArg(7)
    x_0_0_0_sender.send(x_0_0_0)
    None
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
    None
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
        None
def task_2():
    while True:
        var_0 = a_0_0_0_receiver.recv()
        x_0_0_0 = f(var_0)
        x_0_0_0_sender.send(x_0_0_0)
        None
def task_3():
    a_0_0_0 = f(42)
    a_0_0_0_sender.send(a_0_0_0)
    None
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
        None
def task_2():
    a_0_0_0 = f(42)
    a_0_0_0_sender.send(a_0_0_0)
    None
from testLib import *
def algo():
    a = f(42)
    x = f(a)
    g(x)
    return x
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
        None
def task_2():
    while True:
        var_0 = a_0_0_0_receiver.recv()
        x_0_0_0 = f(var_0)
        x_0_0_0_sender.send(x_0_0_0)
        None
def task_3():
    a_0_0_0 = f(42)
    a_0_0_0_sender.send(a_0_0_0)
    None
from testLib import *
def algo():
    a = f(42)
    x = f(a)
    g(x)
    z = f(x)
    return z
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
        None
def task_2():
    b_0_0 = funInt()
    b_0_0_sender.send(b_0_0)
    None
def task_3():
    while True:
        var_0 = b_0_0_receiver.recv()
        a_0_0 = oneArg(var_0)
        a_0_0_sender.send(a_0_0)
        None
def task_4():
    c_0_0 = funInt()
    c_0_0_sender.send(c_0_0)
    None
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
    None
def task_2():
    while True:
        var_0 = x_0_0_0_receiver.recv()
        var_1 = y_0_0_0_receiver.recv()
        c_0_0 = var_0 + var_1
        c_0_0_sender.send(c_0_0)
        None
def task_3():
    x_0_0_0 = f(a)
    x_0_0_0_sender.send(x_0_0_0)
    None
from testLib import *
def algo(a, b):
    x = f(a)
    y = g1(b)
    return x + y
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
    None
from testLib import *
def algo(a, b):
    x = (lambda a: 2 * a)(3)
    return x
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
    None
from testLib import *
def algo(a, b):
    x = lambda a: 2 * a
    y = x(3)
    return y
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


--Test cases for State.hs ---------------------------------------------
callMethod = [pythonModule|
import multiprocessing as mp
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
mob_0_0_1_sender, mob_0_0_1_receiver = mp.Pipe()
def task_1():
    mob_0_0_1 = MObs(22)
    mob_0_0_1_sender.send(mob_0_0_1)
    None
def task_2():
    while True:
        var_0 = mob_0_0_1_receiver.recv()
        x_0_0_0 = var_0.getNum()
        x_0_0_0_sender.send(x_0_0_0)
        None
from testLib import *
def algo():
    mob = MObs(22)
    x = mob.getNum()
    return x
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
iteLiteralArgs = [pythonModule|
from testLib import *

def algo(a): 
    b = a
    if b:
        x = f(13)
    else:
        x = g(14)
    return x
|]

iteParamArgs = [pythonModule|
from testLib import *

def algo(a , b): 
    c = a
    if b:
        x = f(a)
    else:
        x = g(b)
    return x
|]


branchReturn = [pythonModule|
from testLib import *

def algo(i):
    if a:
        d = g0(b)
    else:
        return
    return oneArg(d) 
|] 

iteNoParams = [pythonModule|
from testLib import *

def algo():
    a = f()
    b = f1(a)
    c = f2(a)
    if a:
        d = g0(b)
    else:
        d = g1(c)
    return oneArg(d) 
|]

iteRustExample = [pythonModule|
from testLib import *

def algo(i):
    a = f(i)
    b = f1(i)
    c = f2(i)
    if a:
        d = g0(b)
    else:
        d = g1(c)
    return oneArg(d) 
|]

condExpr = [pythonModule|
from testLib import *

def algo(i):
    return None
    # c = someCheck(i)
    # x = f(i) if c else g(i)
|]


condExpr2 = [pythonModule|
from testLib import *

def algo(i):
    a = f()
    b = f1(a)
    c = f2(a)
    d = g0(b) if a else g1(c)
    return oneArg(d) 
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