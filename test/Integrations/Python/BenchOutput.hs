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


natPar = [pythonModule|
import multiprocessing as mp
result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
c_0_0_0_sender, c_0_0_0_receiver = mp.Pipe()
b_0_0_0_sender, b_0_0_0_receiver = mp.Pipe()
a_0_0_0_sender, a_0_0_0_receiver = mp.Pipe()
def task_1():
    while True:
        var_1 = a_0_0_0_receiver.recv()
        var_2 = b_0_0_0_receiver.recv()
        var_3 = c_0_0_0_receiver.recv()
        result_0_0_0 = combine(i, var_1, var_2, var_3)
        result_0_0_0_sender.send(result_0_0_0)
def task_2():
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_3():
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_4():
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
from lib import *
def main(i_1):
    global i
    i, = i_1,
    tasks = [task_1, task_2, task_3, task_4]
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

natPar7 = [pythonModule|
import multiprocessing as mp
result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
g_0_0_0_sender, g_0_0_0_receiver = mp.Pipe()
f_0_0_0_sender, f_0_0_0_receiver = mp.Pipe()
e_0_0_0_sender, e_0_0_0_receiver = mp.Pipe()
d_0_0_0_sender, d_0_0_0_receiver = mp.Pipe()
c_0_0_0_sender, c_0_0_0_receiver = mp.Pipe()
b_0_0_0_sender, b_0_0_0_receiver = mp.Pipe()
a_0_0_0_sender, a_0_0_0_receiver = mp.Pipe()
def task_1():
    d_0_0_0 = fun4(i)
    d_0_0_0_sender.send(d_0_0_0)
def task_2():
    f_0_0_0 = fun6(i)
    f_0_0_0_sender.send(f_0_0_0)
def task_3():
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_4():
    while True:
        var_1 = a_0_0_0_receiver.recv()
        var_2 = b_0_0_0_receiver.recv()
        var_3 = c_0_0_0_receiver.recv()
        var_4 = d_0_0_0_receiver.recv()
        var_5 = e_0_0_0_receiver.recv()
        var_6 = f_0_0_0_receiver.recv()
        var_7 = g_0_0_0_receiver.recv()
        result_0_0_0 = combine(i, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        result_0_0_0_sender.send(result_0_0_0)
def task_5():
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_6():
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
def task_7():
    e_0_0_0 = fun5(i)
    e_0_0_0_sender.send(e_0_0_0)
def task_8():
    g_0_0_0 = fun7(i)
    g_0_0_0_sender.send(g_0_0_0)
from lib import *
def main(i_1):
    global i
    i, = i_1,
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8]
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

natPar15 = [pythonModule|
import multiprocessing as mp
result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
o_0_0_0_sender, o_0_0_0_receiver = mp.Pipe()
n_0_0_0_sender, n_0_0_0_receiver = mp.Pipe()
m_0_0_0_sender, m_0_0_0_receiver = mp.Pipe()
l_0_0_0_sender, l_0_0_0_receiver = mp.Pipe()
k_0_0_0_sender, k_0_0_0_receiver = mp.Pipe()
j_0_0_0_sender, j_0_0_0_receiver = mp.Pipe()
other_i_0_0_0_sender, other_i_0_0_0_receiver = mp.Pipe()
h_0_0_0_sender, h_0_0_0_receiver = mp.Pipe()
g_0_0_0_sender, g_0_0_0_receiver = mp.Pipe()
f_0_0_0_sender, f_0_0_0_receiver = mp.Pipe()
e_0_0_0_sender, e_0_0_0_receiver = mp.Pipe()
d_0_0_0_sender, d_0_0_0_receiver = mp.Pipe()
c_0_0_0_sender, c_0_0_0_receiver = mp.Pipe()
b_0_0_0_sender, b_0_0_0_receiver = mp.Pipe()
a_0_0_0_sender, a_0_0_0_receiver = mp.Pipe()
def task_1():
    d_0_0_0 = fun4(i)
    d_0_0_0_sender.send(d_0_0_0)
def task_2():
    m_0_0_0 = fun13(i)
    m_0_0_0_sender.send(m_0_0_0)
def task_3():
    o_0_0_0 = fun15(i)
    o_0_0_0_sender.send(o_0_0_0)
def task_4():
    f_0_0_0 = fun6(i)
    f_0_0_0_sender.send(f_0_0_0)
def task_5():
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_6():
    h_0_0_0 = fun8(i)
    h_0_0_0_sender.send(h_0_0_0)
def task_7():
    l_0_0_0 = fun12(i)
    l_0_0_0_sender.send(l_0_0_0)
def task_8():
    k_0_0_0 = fun11(i)
    k_0_0_0_sender.send(k_0_0_0)
def task_9():
    n_0_0_0 = fun14(i)
    n_0_0_0_sender.send(n_0_0_0)
def task_10():
    other_i_0_0_0 = fun9(i)
    other_i_0_0_0_sender.send(other_i_0_0_0)
def task_11():
    j_0_0_0 = fun10(i)
    j_0_0_0_sender.send(j_0_0_0)
def task_12():
    while True:
        var_1 = a_0_0_0_receiver.recv()
        var_2 = b_0_0_0_receiver.recv()
        var_3 = c_0_0_0_receiver.recv()
        var_4 = d_0_0_0_receiver.recv()
        var_5 = e_0_0_0_receiver.recv()
        var_6 = f_0_0_0_receiver.recv()
        var_7 = g_0_0_0_receiver.recv()
        var_8 = h_0_0_0_receiver.recv()
        var_9 = other_i_0_0_0_receiver.recv()
        var_10 = j_0_0_0_receiver.recv()
        var_11 = k_0_0_0_receiver.recv()
        var_12 = l_0_0_0_receiver.recv()
        var_13 = m_0_0_0_receiver.recv()
        var_14 = n_0_0_0_receiver.recv()
        var_15 = o_0_0_0_receiver.recv()
        result_0_0_0 = combine(i, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11, var_12, var_13, var_14, var_15)
        result_0_0_0_sender.send(result_0_0_0)
def task_13():
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_14():
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
def task_15():
    e_0_0_0 = fun5(i)
    e_0_0_0_sender.send(e_0_0_0)
def task_16():
    g_0_0_0 = fun7(i)
    g_0_0_0_sender.send(g_0_0_0)
from lib import *
def main(i_1):
    global i
    i, = i_1,
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9, task_10, task_11, task_12, task_13, task_14, task_15, task_16]
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

natPar31 = [pythonModule|
import multiprocessing as mp
result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
o1_0_0_0_sender, o1_0_0_0_receiver = mp.Pipe()
n1_0_0_0_sender, n1_0_0_0_receiver = mp.Pipe()
m1_0_0_0_sender, m1_0_0_0_receiver = mp.Pipe()
l1_0_0_0_sender, l1_0_0_0_receiver = mp.Pipe()
k1_0_0_0_sender, k1_0_0_0_receiver = mp.Pipe()
j1_0_0_0_sender, j1_0_0_0_receiver = mp.Pipe()
i1_0_0_0_sender, i1_0_0_0_receiver = mp.Pipe()
h1_0_0_0_sender, h1_0_0_0_receiver = mp.Pipe()
g1_0_0_0_sender, g1_0_0_0_receiver = mp.Pipe()
f1_0_0_0_sender, f1_0_0_0_receiver = mp.Pipe()
e1_0_0_0_sender, e1_0_0_0_receiver = mp.Pipe()
d1_0_0_0_sender, d1_0_0_0_receiver = mp.Pipe()
c1_0_0_0_sender, c1_0_0_0_receiver = mp.Pipe()
b1_0_0_0_sender, b1_0_0_0_receiver = mp.Pipe()
a1_0_0_0_sender, a1_0_0_0_receiver = mp.Pipe()
o_0_0_0_sender, o_0_0_0_receiver = mp.Pipe()
n_0_0_0_sender, n_0_0_0_receiver = mp.Pipe()
m_0_0_0_sender, m_0_0_0_receiver = mp.Pipe()
l_0_0_0_sender, l_0_0_0_receiver = mp.Pipe()
k_0_0_0_sender, k_0_0_0_receiver = mp.Pipe()
j_0_0_0_sender, j_0_0_0_receiver = mp.Pipe()
other_i_0_0_0_sender, other_i_0_0_0_receiver = mp.Pipe()
h_0_0_0_sender, h_0_0_0_receiver = mp.Pipe()
g_0_0_0_sender, g_0_0_0_receiver = mp.Pipe()
f_0_0_0_sender, f_0_0_0_receiver = mp.Pipe()
e_0_0_0_sender, e_0_0_0_receiver = mp.Pipe()
d_0_0_0_sender, d_0_0_0_receiver = mp.Pipe()
c_0_0_0_sender, c_0_0_0_receiver = mp.Pipe()
b_0_0_0_sender, b_0_0_0_receiver = mp.Pipe()
a_0_0_0_sender, a_0_0_0_receiver = mp.Pipe()
def task_1():
    l1_0_0_0 = fun12(i)
    l1_0_0_0_sender.send(l1_0_0_0)
def task_2():
    d_0_0_0 = fun4(i)
    d_0_0_0_sender.send(d_0_0_0)
def task_3():
    d1_0_0_0 = fun4(i)
    d1_0_0_0_sender.send(d1_0_0_0)
def task_4():
    k1_0_0_0 = fun11(i)
    k1_0_0_0_sender.send(k1_0_0_0)
def task_5():
    m_0_0_0 = fun13(i)
    m_0_0_0_sender.send(m_0_0_0)
def task_6():
    o_0_0_0 = fun15(i)
    o_0_0_0_sender.send(o_0_0_0)
def task_7():
    j1_0_0_0 = fun10(i)
    j1_0_0_0_sender.send(j1_0_0_0)
def task_8():
    n1_0_0_0 = fun14(i)
    n1_0_0_0_sender.send(n1_0_0_0)
def task_9():
    f_0_0_0 = fun6(i)
    f_0_0_0_sender.send(f_0_0_0)
def task_10():
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_11():
    while True:
        var_1 = a_0_0_0_receiver.recv()
        var_2 = b_0_0_0_receiver.recv()
        var_3 = c_0_0_0_receiver.recv()
        var_4 = d_0_0_0_receiver.recv()
        var_5 = e_0_0_0_receiver.recv()
        var_6 = f_0_0_0_receiver.recv()
        var_7 = g_0_0_0_receiver.recv()
        var_8 = h_0_0_0_receiver.recv()
        var_9 = other_i_0_0_0_receiver.recv()
        var_10 = j_0_0_0_receiver.recv()
        var_11 = k_0_0_0_receiver.recv()
        var_12 = l_0_0_0_receiver.recv()
        var_13 = m_0_0_0_receiver.recv()
        var_14 = n_0_0_0_receiver.recv()
        var_15 = o_0_0_0_receiver.recv()
        var_16 = a1_0_0_0_receiver.recv()
        var_17 = b1_0_0_0_receiver.recv()
        var_18 = c1_0_0_0_receiver.recv()
        var_19 = d1_0_0_0_receiver.recv()
        var_20 = e1_0_0_0_receiver.recv()
        var_21 = f1_0_0_0_receiver.recv()
        var_22 = g1_0_0_0_receiver.recv()
        var_23 = h1_0_0_0_receiver.recv()
        var_24 = i1_0_0_0_receiver.recv()
        var_25 = j1_0_0_0_receiver.recv()
        var_26 = k1_0_0_0_receiver.recv()
        var_27 = l1_0_0_0_receiver.recv()
        var_28 = m1_0_0_0_receiver.recv()
        var_29 = n1_0_0_0_receiver.recv()
        var_30 = o1_0_0_0_receiver.recv()
        result_0_0_0 = combine(i, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11, var_12, var_13, var_14, var_15, var_16, var_17, var_18, var_19, var_20, var_21, var_22, var_23, var_24, var_25, var_26, var_27, var_28, var_29, var_30)
        result_0_0_0_sender.send(result_0_0_0)
def task_12():
    f1_0_0_0 = fun6(i)
    f1_0_0_0_sender.send(f1_0_0_0)
def task_13():
    b1_0_0_0 = fun2(i)
    b1_0_0_0_sender.send(b1_0_0_0)
def task_14():
    h_0_0_0 = fun8(i)
    h_0_0_0_sender.send(h_0_0_0)
def task_15():
    h1_0_0_0 = fun8(i)
    h1_0_0_0_sender.send(h1_0_0_0)
def task_16():
    l_0_0_0 = fun12(i)
    l_0_0_0_sender.send(l_0_0_0)
def task_17():
    o1_0_0_0 = fun15(i)
    o1_0_0_0_sender.send(o1_0_0_0)
def task_18():
    m1_0_0_0 = fun13(i)
    m1_0_0_0_sender.send(m1_0_0_0)
def task_19():
    k_0_0_0 = fun11(i)
    k_0_0_0_sender.send(k_0_0_0)
def task_20():
    n_0_0_0 = fun14(i)
    n_0_0_0_sender.send(n_0_0_0)
def task_21():
    other_i_0_0_0 = fun9(i)
    other_i_0_0_0_sender.send(other_i_0_0_0)
def task_22():
    j_0_0_0 = fun10(i)
    j_0_0_0_sender.send(j_0_0_0)
def task_23():
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_24():
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
def task_25():
    e_0_0_0 = fun5(i)
    e_0_0_0_sender.send(e_0_0_0)
def task_26():
    g_0_0_0 = fun7(i)
    g_0_0_0_sender.send(g_0_0_0)
def task_27():
    a1_0_0_0 = fun1(i)
    a1_0_0_0_sender.send(a1_0_0_0)
def task_28():
    c1_0_0_0 = fun3(i)
    c1_0_0_0_sender.send(c1_0_0_0)
def task_29():
    g1_0_0_0 = fun7(i)
    g1_0_0_0_sender.send(g1_0_0_0)
def task_30():
    i1_0_0_0 = fun9(i)
    i1_0_0_0_sender.send(i1_0_0_0)
def task_31():
    e1_0_0_0 = fun5(i)
    e1_0_0_0_sender.send(e1_0_0_0)
from lib import *
def main(i_1):
    global i
    i, = i_1,
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9, task_10, task_11, task_12, task_13, task_14, task_15, task_16, task_17, task_18, task_19, task_20, task_21, task_22, task_23, task_24, task_25, task_26, task_27, task_28, task_29, task_30, task_31]
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
