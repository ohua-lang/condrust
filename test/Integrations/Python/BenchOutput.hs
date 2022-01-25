{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.BenchOutput where

import Integrations.Python.SimpleQuoter


blackScholes = [pythonModule|
import multiprocessing as mp
results_0_1_0_sender, results_0_1_0_receiver = mp.Pipe()
results_0_0_1_sender, results_0_0_1_receiver = mp.Pipe()
ctrl_0_0_sender, ctrl_0_0_receiver = mp.Pipe()
d_0_sender, d_0_receiver = mp.Pipe()
n_0_0_0_sender, n_0_0_0_receiver = mp.Pipe()
def task_1():
    results_0_0_1 = []
    results_0_0_1_sender.send(results_0_0_1)
def task_2():
    while True:
        var_0 = d_0_receiver.recv()
        n_0_0_0 = calulateForOption(var_0)
        n_0_0_0_sender.send(n_0_0_0)
def task_3():
    valOps_0_0_0 = refl(ops)
    while True:
        hasSize = True if hasattr(valOps_0_0_0, '__len__') else False
        if hasSize:
            size = len(valOps_0_0_0)
            ctrl = True, size
            ctrl_0_0_sender.send(ctrl)
            for d in valOps_0_0_0:
                d_0_sender.send(d)
        else:
            size = 0
            for d in valOps_0_0_0:
                d_0_sender.send(d)
                ctrl = False, 1
                ctrl_0_0_sender.send(ctrl)
                size = size + 1
            ctrl = True, 0
            ctrl_0_0_sender.send(ctrl)
def task_4():
    while True:
        renew = False
        results_0_0_1_0 = results_0_0_1_receiver.recv()
        while not renew:
            sig = ctrl_0_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                var_1 = n_0_0_0_receiver.recv()
                results_0_0_1_0.append(var_1)
            renew_next_time = sig[0]
            renew = renew_next_time
        results_0_1_0_sender.send(results_0_0_1_0)
from bs_lib import (calulateForOption, refl)
def main(ops_1):
    global ops
    ops, = ops_1,
    tasks = [task_1, task_2, task_3, task_4]
    processes = []
    for task in tasks:
        process = mp.Process(target=task)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = results_0_1_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

nBodies = [pythonModule|
from lib import *
# Todo
|]



loop3 = [pythonModule|
import multiprocessing as mp
def task_1(x_0_0_0_sender, d_1_receiver):
    while True:
        var_0 = d_1_receiver.recv()
        x_0_0_0 = fun1(var_0)
        x_0_0_0_sender.send(x_0_0_0)
def task_2(z_0_0_0_sender, y_0_0_0_receiver):
    while True:
        var_0 = y_0_0_0_receiver.recv()
        z_0_0_0 = fun3(var_0)
        z_0_0_0_sender.send(z_0_0_0)
def task_3(y_0_0_0_sender, x_0_0_0_receiver):
    while True:
        var_0 = x_0_0_0_receiver.recv()
        y_0_0_0 = fun2(var_0)
        y_0_0_0_sender.send(y_0_0_0)
def task_4(ctrl_0_0_sender, d_1_sender):
    a_0_0 = range(0, i)
    while True:
        hasSize = True if hasattr(a_0_0, '__len__') else False
        if hasSize:
            size = len(a_0_0)
            ctrl = True, size
            ctrl_0_0_sender.send(ctrl)
            for d in a_0_0:
                d_1_sender.send(d)
        else:
            size = 0
            for d in a_0_0:
                d_1_sender.send(d)
                ctrl = False, 1
                ctrl_0_0_sender.send(ctrl)
                size = size + 1
            ctrl = True, 0
            ctrl_0_0_sender.send(ctrl)
def task_5(result_0_0_1_sender):
    result_0_0_1 = []
    result_0_0_1_sender.send(result_0_0_1)
def task_6(result_0_1_0_sender, result_0_0_1_receiver, ctrl_0_0_receiver, z_0_0_0_receiver):
    while True:
        renew = False
        result_0_0_1_0 = result_0_0_1_receiver.recv()
        while not renew:
            sig = ctrl_0_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                var_1 = z_0_0_0_receiver.recv()
                result_0_0_1_0.append(var_1)
            renew_next_time = sig[0]
            renew = renew_next_time
        result_0_1_0_sender.send(result_0_0_1_0)
from helpers.library_proxy import *
def main(i_1):
    global i
    i, = i_1,
    result_0_1_0_sender, result_0_1_0_receiver = mp.Pipe()
    result_0_0_1_sender, result_0_0_1_receiver = mp.Pipe()
    ctrl_0_0_sender, ctrl_0_0_receiver = mp.Pipe()
    d_1_sender, d_1_receiver = mp.Pipe()
    x_0_0_0_sender, x_0_0_0_receiver = mp.Pipe()
    y_0_0_0_sender, y_0_0_0_receiver = mp.Pipe()
    z_0_0_0_sender, z_0_0_0_receiver = mp.Pipe()
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6]
    channels = [[x_0_0_0_sender, d_1_receiver], [z_0_0_0_sender, y_0_0_0_receiver], [y_0_0_0_sender, x_0_0_0_receiver], [ctrl_0_0_sender, d_1_sender], [result_0_0_1_sender], [result_0_1_0_sender, result_0_0_1_receiver, ctrl_0_0_receiver, z_0_0_0_receiver]]
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

ifElse =  [pythonModule|
import multiprocessing as mp
def task_1(c_0_0_sender, ctrlFalse_0_receiver):
    while True:
        renew = False
        while not renew:
            sig = ctrlFalse_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                c_0_0 = fun2(i)
                c_0_0_sender.send(c_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_2(result_0_sender, a_0_0_1_receiver, b_0_0_receiver, c_0_0_receiver):
    while True:
        branchSelection = a_0_0_1_receiver.recv()
        if branchSelection:
            result = b_0_0_receiver.recv()
            result_0_sender.send(result)
        else:
            result = c_0_0_receiver.recv()
            result_0_sender.send(result)
def task_3(b_0_0_sender, ctrlTrue_0_receiver):
    while True:
        renew = False
        while not renew:
            sig = ctrlTrue_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                b_0_0 = fun1(i)
                b_0_0_sender.send(b_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_4(ctrlTrue_0_sender, ctrlFalse_0_sender, a_0_0_0_receiver):
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
def task_5(a_0_0_0_sender, a_0_0_1_sender):
    res = check(i)
    a_0_0_0_sender.send(res)
    a_0_0_1_sender.send(res)
from helpers.library_proxy import *
def main(i_1):
    global i
    i, = i_1,
    result_0_sender, result_0_receiver = mp.Pipe()
    a_0_0_0_sender, a_0_0_0_receiver = mp.Pipe()
    ctrlTrue_0_sender, ctrlTrue_0_receiver = mp.Pipe()
    ctrlFalse_0_sender, ctrlFalse_0_receiver = mp.Pipe()
    c_0_0_sender, c_0_0_receiver = mp.Pipe()
    b_0_0_sender, b_0_0_receiver = mp.Pipe()
    a_0_0_1_sender, a_0_0_1_receiver = mp.Pipe()
    tasks = [task_1, task_2, task_3, task_4, task_5]
    channels = [[c_0_0_sender, ctrlFalse_0_receiver], [result_0_sender, a_0_0_1_receiver, b_0_0_receiver, c_0_0_receiver], [b_0_0_sender, ctrlTrue_0_receiver], [ctrlTrue_0_sender, ctrlFalse_0_sender, a_0_0_0_receiver], [a_0_0_0_sender, a_0_0_1_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

ifElseLikeTailRec = [pythonModule|
import multiprocessing as mp
def task_1(e_0_0_sender, current_0_0_0_2_receiver, ctrlTrue_0_receiver):
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
def task_2(ctrl_0_0_sender, d_1_sender):
    b_0_0 = range(i)
    while True:
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
def task_3(flag_0_0_0_sender, c1_0_0_0_receiver):
    while True:
        var_0 = c1_0_0_0_receiver.recv()
        flag_0_0_0 = fun1(var_0)
        flag_0_0_0_sender.send(flag_0_0_0)
def task_4(result_0_0_1_sender):
    result_0_0_1 = []
    result_0_0_1_sender.send(result_0_0_1)
def task_5(c_0_0_0_sender, c_0_0_1_sender, flag_0_0_0_receiver):
    while True:
        var_0 = flag_0_0_0_receiver.recv()
        res = check(var_0)
        c_0_0_0_sender.send(res)
        c_0_0_1_sender.send(res)
def task_6(c1_0_0_0_sender, c2_0_0_0_sender, d_1_receiver):
    while True:
        var_0 = d_1_receiver.recv()
        res = double(var_0)
        c1_0_0_0 = res[0]
        c1_0_0_0_sender.send(c1_0_0_0)
        c2_0_0_0 = res[1]
        c2_0_0_0_sender.send(c2_0_0_0)
def task_7(ctrlTrue_0_sender, ctrlFalse_0_sender, c_0_0_0_receiver):
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
def task_8(current_0_0_0_2_sender, current_0_0_0_3_sender, c2_0_0_0_receiver):
    while True:
        var_0 = c2_0_0_0_receiver.recv()
        res = fun1(var_0)
        current_0_0_0_2_sender.send(res)
        current_0_0_0_3_sender.send(res)
def task_9(result_2_sender, c_0_0_1_receiver, e_0_0_receiver, g_0_0_receiver):
    while True:
        branchSelection = c_0_0_1_receiver.recv()
        if branchSelection:
            result = e_0_0_receiver.recv()
            result_2_sender.send(result)
        else:
            result = g_0_0_receiver.recv()
            result_2_sender.send(result)
def task_10(g_0_0_sender, current_0_0_0_3_receiver, ctrlFalse_0_receiver):
    while True:
        renew = False
        current_0_0_0_1 = current_0_0_0_3_receiver.recv()
        while not renew:
            sig = ctrlFalse_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                g_0_0 = ohua.lang.id(current_0_0_0_1)
                g_0_0_sender.send(g_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
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
from helpers.library_proxy import *
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
    channels = [[e_0_0_sender, current_0_0_0_2_receiver, ctrlTrue_0_receiver], [ctrl_0_0_sender, d_1_sender], [flag_0_0_0_sender, c1_0_0_0_receiver], [result_0_0_1_sender], [c_0_0_0_sender, c_0_0_1_sender, flag_0_0_0_receiver], [c1_0_0_0_sender, c2_0_0_0_sender, d_1_receiver], [ctrlTrue_0_sender, ctrlFalse_0_sender, c_0_0_0_receiver], [current_0_0_0_2_sender, current_0_0_0_3_sender, c2_0_0_0_receiver], [result_2_sender, c_0_0_1_receiver, e_0_0_receiver, g_0_0_receiver], [g_0_0_sender, current_0_0_0_3_receiver, ctrlFalse_0_receiver], [result_0_1_0_sender, result_0_0_1_receiver, ctrl_0_0_receiver, result_2_receiver]]
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

ifElseLoopStates'' = [pythonModule|
import multiprocessing as mp
def task_1(c_0_0_sender, var_0_0_0_3_receiver, ctrlTrue_0_receiver):
    while True:
        renew = False
        var_0_0_0_0 = var_0_0_0_3_receiver.recv()
        while not renew:
            sig = ctrlTrue_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                c_0_0 = fun1(var_0_0_0_0)
                c_0_0_sender.send(c_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_2(b_0_0_0_sender, b_0_0_1_sender, var_0_0_0_2_receiver):
    while True:
        var_0 = var_0_0_0_2_receiver.recv()
        res = check(var_0)
        b_0_0_0_sender.send(res)
        b_0_0_1_sender.send(res)
def task_3(result_0_0_1_sender):
    result_0_0_1 = []
    result_0_0_1_sender.send(result_0_0_1)
def task_4(e_0_0_sender, var_0_0_0_receiver, ctrlFalse_0_receiver):
    while True:
        renew = False
        var_0_0_0_1 = var_0_0_0_receiver.recv()
        while not renew:
            sig = ctrlFalse_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                e_0_0 = fun2(var_0_0_0_1)
                e_0_0_sender.send(e_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
def task_5(ctrlTrue_0_sender, ctrlFalse_0_sender, b_0_0_0_receiver):
    while True:
        branchSelection = b_0_0_0_receiver.recv()
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
def task_6(var_0_0_0_2_sender, var_0_0_0_3_sender, d_1_receiver):
    while True:
        var_0 = d_1_receiver.recv()
        res = var_0.method()
        var_0_0_0_2_sender.send(res)
        var_0_0_0_3_sender.send(res)
def task_7(ctrl_0_0_sender, d_1_sender):
    a_0_0 = list(statefulObjs)
    while True:
        hasSize = True if hasattr(a_0_0, '__len__') else False
        if hasSize:
            size = len(a_0_0)
            ctrl = True, size
            ctrl_0_0_sender.send(ctrl)
            for d in a_0_0:
                d_1_sender.send(d)
        else:
            size = 0
            for d in a_0_0:
                d_1_sender.send(d)
                ctrl = False, 1
                ctrl_0_0_sender.send(ctrl)
                size = size + 1
            ctrl = True, 0
            ctrl_0_0_sender.send(ctrl)
def task_8(result_2_sender, b_0_0_1_receiver, c_0_0_receiver, e_0_0_receiver):
    while True:
        branchSelection = b_0_0_1_receiver.recv()
        if branchSelection:
            result = c_0_0_receiver.recv()
            result_2_sender.send(result)
        else:
            result = e_0_0_receiver.recv()
            result_2_sender.send(result)
def task_9(result_0_1_0_sender, result_0_0_1_receiver, ctrl_0_0_receiver, result_2_receiver):
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
from helpers.library_proxy import *
def main(statefulObjs_1):
    global statefulObjs
    statefulObjs, = statefulObjs_1,
    result_0_1_0_sender, result_0_1_0_receiver = mp.Pipe()
    result_0_0_1_sender, result_0_0_1_receiver = mp.Pipe()
    ctrl_0_0_sender, ctrl_0_0_receiver = mp.Pipe()
    d_1_sender, d_1_receiver = mp.Pipe()
    var_0_0_0_2_sender, var_0_0_0_2_receiver = mp.Pipe()
    b_0_0_0_sender, b_0_0_0_receiver = mp.Pipe()
    var_0_0_0_3_sender, var_0_0_0_3_receiver = mp.Pipe()
    ctrlTrue_0_sender, ctrlTrue_0_receiver = mp.Pipe()
    var_0_0_0_sender, var_0_0_0_receiver = mp.Pipe()
    ctrlFalse_0_sender, ctrlFalse_0_receiver = mp.Pipe()
    e_0_0_sender, e_0_0_receiver = mp.Pipe()
    c_0_0_sender, c_0_0_receiver = mp.Pipe()
    b_0_0_1_sender, b_0_0_1_receiver = mp.Pipe()
    result_2_sender, result_2_receiver = mp.Pipe()
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9]
    channels = [[c_0_0_sender, var_0_0_0_3_receiver, ctrlTrue_0_receiver], [b_0_0_0_sender, b_0_0_1_sender, var_0_0_0_2_receiver], [result_0_0_1_sender], [e_0_0_sender, var_0_0_0_receiver, ctrlFalse_0_receiver], [ctrlTrue_0_sender, ctrlFalse_0_sender, b_0_0_0_receiver], [var_0_0_0_2_sender, var_0_0_0_3_sender, d_1_receiver], [ctrl_0_0_sender, d_1_sender], [result_2_sender, b_0_0_1_receiver, c_0_0_receiver, e_0_0_receiver], [result_0_1_0_sender, result_0_0_1_receiver, ctrl_0_0_receiver, result_2_receiver]]
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

ifElseLikeTailRec' = [pythonModule|
import multiprocessing as mp
def task_1(e_0_0_sender, current_0_0_0_2_receiver, ctrlTrue_0_receiver):
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
def task_2(flag_0_0_0_sender, c1_0_0_0_receiver):
    while True:
        var_0 = c1_0_0_0_receiver.recv()
        flag_0_0_0 = fun1(var_0)
        flag_0_0_0_sender.send(flag_0_0_0)
def task_3(result_0_0_1_sender):
    result_0_0_1 = []
    result_0_0_1_sender.send(result_0_0_1)
def task_4(result_2_sender, c_0_0_1_receiver, e_0_0_receiver, f_0_0_receiver):
    while True:
        branchSelection = c_0_0_1_receiver.recv()
        if branchSelection:
            result = e_0_0_receiver.recv()
            result_2_sender.send(result)
        else:
            result = f_0_0_receiver.recv()
            result_2_sender.send(result)
def task_5(c_0_0_0_sender, c_0_0_1_sender, flag_0_0_0_receiver):
    while True:
        var_0 = flag_0_0_0_receiver.recv()
        res = check(var_0)
        c_0_0_0_sender.send(res)
        c_0_0_1_sender.send(res)
def task_6(ctrl_0_0_sender, d_1_sender):
    b_0_0 = list(i)
    while True:
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
def task_7(c1_0_0_0_sender, c2_0_0_0_sender, d_1_receiver):
    while True:
        var_0 = d_1_receiver.recv()
        res = double(var_0)
        c1_0_0_0 = res[0]
        c1_0_0_0_sender.send(c1_0_0_0)
        c2_0_0_0 = res[1]
        c2_0_0_0_sender.send(c2_0_0_0)
def task_8(ctrlTrue_0_sender, ctrlFalse_0_sender, c_0_0_0_receiver):
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
def task_9(current_0_0_0_2_sender, current_0_0_0_3_sender, c2_0_0_0_receiver):
    while True:
        var_0 = c2_0_0_0_receiver.recv()
        res = fun1(var_0)
        current_0_0_0_2_sender.send(res)
        current_0_0_0_3_sender.send(res)
def task_10(f_0_0_sender, current_0_0_0_3_receiver, ctrlFalse_0_receiver):
    while True:
        renew = False
        current_0_0_0_1 = current_0_0_0_3_receiver.recv()
        while not renew:
            sig = ctrlFalse_0_receiver.recv()
            count = sig[1]
            for _ in range(0, count):
                f_0_0 = fun3(current_0_0_0_1)
                f_0_0_sender.send(f_0_0)
            renew_next_time = sig[0]
            renew = renew_next_time
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
from helpers.library_proxy import *
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
    f_0_0_sender, f_0_0_receiver = mp.Pipe()
    e_0_0_sender, e_0_0_receiver = mp.Pipe()
    c_0_0_1_sender, c_0_0_1_receiver = mp.Pipe()
    result_2_sender, result_2_receiver = mp.Pipe()
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9, task_10, task_11]
    channels = [[e_0_0_sender, current_0_0_0_2_receiver, ctrlTrue_0_receiver], [flag_0_0_0_sender, c1_0_0_0_receiver], [result_0_0_1_sender], [result_2_sender, c_0_0_1_receiver, e_0_0_receiver, f_0_0_receiver], [c_0_0_0_sender, c_0_0_1_sender, flag_0_0_0_receiver], [ctrl_0_0_sender, d_1_sender], [c1_0_0_0_sender, c2_0_0_0_sender, d_1_receiver], [ctrlTrue_0_sender, ctrlFalse_0_sender, c_0_0_0_receiver], [current_0_0_0_2_sender, current_0_0_0_3_sender, c2_0_0_0_receiver], [f_0_0_sender, current_0_0_0_3_receiver, ctrlFalse_0_receiver], [result_0_1_0_sender, result_0_0_1_receiver, ctrl_0_0_receiver, result_2_receiver]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_1_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result|]