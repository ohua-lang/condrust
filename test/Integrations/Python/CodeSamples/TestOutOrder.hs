{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.CodeSamples.TestOutOrder where

import Integrations.Python.CodeSamples.SimpleQuoter

callAFunction = [pythonModule|
import multiprocessing as mp
a_0_0_sender, a_0_0_receiver = mp.Pipe()
x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
def task_1():
    x_0_0_0_receiver.recv()
    a_0_0_sender.send(None)
def task_2():
    x_0_0_0 = hello_world()
    x_0_0_0_sender.send(x_0_0_0)
from testLib import *
def main():
    tasks = [task_1, task_2]
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

assignBinOpChained  = [pythonModule|
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
    z_0_0_0 = g()
    z_0_0_0_sender.send(z_0_0_0)
def task_3():
    x_0_0_0 = f()
    x_0_0_0_sender.send(x_0_0_0)
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
    c_0_0 = funInt()
    c_0_0_sender.send(c_0_0)
def task_3():
    while True:
        var_0 = b_0_0_receiver.recv()
        a_0_0 = oneArg(var_0)
        a_0_0_sender.send(a_0_0)
def task_4():
    b_0_0 = funInt()
    b_0_0_sender.send(b_0_0)
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


assignToSubscript = [pythonModule|
import multiprocessing as mp
l_0_0_0_0_sender, l_0_0_0_0_receiver = mp.Pipe()
a_1_0_sender, a_1_0_receiver = mp.Pipe()
l_0_0_1_sender, l_0_0_1_receiver = mp.Pipe()
def task_1():
    while True:
        var_0 = l_0_0_1_receiver.recv()
        var_2 = a_1_0_receiver.recv()
        var_0[1] = var_2
        l_0_0_0_0_sender.send(var_0)
def task_2():
    a_1_0 = f()
    a_1_0_sender.send(a_1_0)
def task_3():
    l_0_0_1 = [1, 2, 3]
    l_0_0_1_sender.send(l_0_0_1)
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
    result = l_0_0_0_0_receiver.recv()
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
def task_2():
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
def task_3():
    while True:
        var_0 = mob_0_0_2_receiver.recv()
        mob2_0_0_0 = var_0.clone()
        mob2_0_0_0_sender.send(mob2_0_0_0)
        mob_0_0_1_0_sender.send(var_0)
def task_4():
    mob_0_0_2 = MObs(a)
    mob_0_0_2_sender.send(mob_0_0_2)
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
def task_3():
    s_0_0_1 = MObs(42)
    s_0_0_1_sender.send(s_0_0_1)
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
