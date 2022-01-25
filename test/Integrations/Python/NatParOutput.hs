{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.NatParOutput where
import Integrations.Python.SimpleQuoter


natPar4 = [pythonModule|
import multiprocessing as mp
def task_1(d_0_0_0_sender):
    d_0_0_0 = fun4(i)
    d_0_0_0_sender.send(d_0_0_0)
def task_2(result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver):
    while True:
        var_1 = a_0_0_0_receiver.recv()
        var_2 = b_0_0_0_receiver.recv()
        var_3 = c_0_0_0_receiver.recv()
        var_4 = d_0_0_0_receiver.recv()
        result_0_0_0 = combine(i, var_1, var_2, var_3, var_4)
        result_0_0_0_sender.send(result_0_0_0)
def task_3(b_0_0_0_sender):
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_4(c_0_0_0_sender):
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_5(a_0_0_0_sender):
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
from helpers.library_proxy import *
def main(i_1):
    global i
    i, = i_1,
    result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
    d_0_0_0_sender, d_0_0_0_receiver = mp.Pipe()
    c_0_0_0_sender, c_0_0_0_receiver = mp.Pipe()
    b_0_0_0_sender, b_0_0_0_receiver = mp.Pipe()
    a_0_0_0_sender, a_0_0_0_receiver = mp.Pipe()
    tasks = [task_1, task_2, task_3, task_4, task_5]
    channels = [[d_0_0_0_sender], [result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver], [b_0_0_0_sender], [c_0_0_0_sender], [a_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

natPar8 = [pythonModule|
import multiprocessing as mp
def task_1(d_0_0_0_sender):
    d_0_0_0 = fun4(i)
    d_0_0_0_sender.send(d_0_0_0)
def task_2(f_0_0_0_sender):
    f_0_0_0 = fun6(i)
    f_0_0_0_sender.send(f_0_0_0)
def task_3(b_0_0_0_sender):
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_4(h_0_0_0_sender):
    h_0_0_0 = fun8(i)
    h_0_0_0_sender.send(h_0_0_0)
def task_5(result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver):
    while True:
        var_1 = a_0_0_0_receiver.recv()
        var_2 = b_0_0_0_receiver.recv()
        var_3 = c_0_0_0_receiver.recv()
        var_4 = d_0_0_0_receiver.recv()
        var_5 = e_0_0_0_receiver.recv()
        var_6 = f_0_0_0_receiver.recv()
        var_7 = g_0_0_0_receiver.recv()
        var_8 = h_0_0_0_receiver.recv()
        result_0_0_0 = combine(i, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8)
        result_0_0_0_sender.send(result_0_0_0)
def task_6(c_0_0_0_sender):
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_7(a_0_0_0_sender):
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
def task_8(e_0_0_0_sender):
    e_0_0_0 = fun5(i)
    e_0_0_0_sender.send(e_0_0_0)
def task_9(g_0_0_0_sender):
    g_0_0_0 = fun7(i)
    g_0_0_0_sender.send(g_0_0_0)
from helpers.library_proxy import *
def main(i_1):
    global i
    i, = i_1,
    result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
    h_0_0_0_sender, h_0_0_0_receiver = mp.Pipe()
    g_0_0_0_sender, g_0_0_0_receiver = mp.Pipe()
    f_0_0_0_sender, f_0_0_0_receiver = mp.Pipe()
    e_0_0_0_sender, e_0_0_0_receiver = mp.Pipe()
    d_0_0_0_sender, d_0_0_0_receiver = mp.Pipe()
    c_0_0_0_sender, c_0_0_0_receiver = mp.Pipe()
    b_0_0_0_sender, b_0_0_0_receiver = mp.Pipe()
    a_0_0_0_sender, a_0_0_0_receiver = mp.Pipe()
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9]
    channels = [[d_0_0_0_sender], [f_0_0_0_sender], [b_0_0_0_sender], [h_0_0_0_sender], [result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver], [c_0_0_0_sender], [a_0_0_0_sender], [e_0_0_0_sender], [g_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

natPar12 = [pythonModule|
import multiprocessing as mp
def task_1(d_0_0_0_sender):
    d_0_0_0 = fun4(i)
    d_0_0_0_sender.send(d_0_0_0)
def task_2(f_0_0_0_sender):
    f_0_0_0 = fun6(i)
    f_0_0_0_sender.send(f_0_0_0)
def task_3(b_0_0_0_sender):
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_4(result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver):
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
        result_0_0_0 = combine(i, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11, var_12)
        result_0_0_0_sender.send(result_0_0_0)
def task_5(h_0_0_0_sender):
    h_0_0_0 = fun8(i)
    h_0_0_0_sender.send(h_0_0_0)
def task_6(l_0_0_0_sender):
    l_0_0_0 = fun12(i)
    l_0_0_0_sender.send(l_0_0_0)
def task_7(k_0_0_0_sender):
    k_0_0_0 = fun11(i)
    k_0_0_0_sender.send(k_0_0_0)
def task_8(other_i_0_0_0_sender):
    other_i_0_0_0 = fun9(i)
    other_i_0_0_0_sender.send(other_i_0_0_0)
def task_9(j_0_0_0_sender):
    j_0_0_0 = fun10(i)
    j_0_0_0_sender.send(j_0_0_0)
def task_10(c_0_0_0_sender):
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_11(a_0_0_0_sender):
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
def task_12(e_0_0_0_sender):
    e_0_0_0 = fun5(i)
    e_0_0_0_sender.send(e_0_0_0)
def task_13(g_0_0_0_sender):
    g_0_0_0 = fun7(i)
    g_0_0_0_sender.send(g_0_0_0)
from helpers.library_proxy import *
def main(i_1):
    global i
    i, = i_1,
    result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
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
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9, task_10, task_11, task_12, task_13]
    channels = [[d_0_0_0_sender], [f_0_0_0_sender], [b_0_0_0_sender], [result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver], [h_0_0_0_sender], [l_0_0_0_sender], [k_0_0_0_sender], [other_i_0_0_0_sender], [j_0_0_0_sender], [c_0_0_0_sender], [a_0_0_0_sender], [e_0_0_0_sender], [g_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
    |]

natPar16 = [pythonModule|
import multiprocessing as mp
def task_1(d_0_0_0_sender):
    d_0_0_0 = fun4(i)
    d_0_0_0_sender.send(d_0_0_0)
def task_2(m_0_0_0_sender):
    m_0_0_0 = fun13(i)
    m_0_0_0_sender.send(m_0_0_0)
def task_3(o_0_0_0_sender):
    o_0_0_0 = fun15(i)
    o_0_0_0_sender.send(o_0_0_0)
def task_4(f_0_0_0_sender):
    f_0_0_0 = fun6(i)
    f_0_0_0_sender.send(f_0_0_0)
def task_5(b_0_0_0_sender):
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_6(h_0_0_0_sender):
    h_0_0_0 = fun8(i)
    h_0_0_0_sender.send(h_0_0_0)
def task_7(l_0_0_0_sender):
    l_0_0_0 = fun12(i)
    l_0_0_0_sender.send(l_0_0_0)
def task_8(k_0_0_0_sender):
    k_0_0_0 = fun11(i)
    k_0_0_0_sender.send(k_0_0_0)
def task_9(n_0_0_0_sender):
    n_0_0_0 = fun14(i)
    n_0_0_0_sender.send(n_0_0_0)
def task_10(other_i_0_0_0_sender):
    other_i_0_0_0 = fun9(i)
    other_i_0_0_0_sender.send(other_i_0_0_0)
def task_11(j_0_0_0_sender):
    j_0_0_0 = fun10(i)
    j_0_0_0_sender.send(j_0_0_0)
def task_12(result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver, m_0_0_0_receiver, n_0_0_0_receiver, o_0_0_0_receiver, a1_0_0_0_receiver):
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
        result_0_0_0 = combine(i, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11, var_12, var_13, var_14, var_15, var_16)
        result_0_0_0_sender.send(result_0_0_0)
def task_13(c_0_0_0_sender):
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_14(a_0_0_0_sender):
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
def task_15(e_0_0_0_sender):
    e_0_0_0 = fun5(i)
    e_0_0_0_sender.send(e_0_0_0)
def task_16(g_0_0_0_sender):
    g_0_0_0 = fun7(i)
    g_0_0_0_sender.send(g_0_0_0)
def task_17(a1_0_0_0_sender):
    a1_0_0_0 = fun1(i)
    a1_0_0_0_sender.send(a1_0_0_0)
from helpers.library_proxy import *
def main(i_1):
    global i
    i, = i_1,
    result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
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
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9, task_10, task_11, task_12, task_13, task_14, task_15, task_16, task_17]
    channels = [[d_0_0_0_sender], [m_0_0_0_sender], [o_0_0_0_sender], [f_0_0_0_sender], [b_0_0_0_sender], [h_0_0_0_sender], [l_0_0_0_sender], [k_0_0_0_sender], [n_0_0_0_sender], [other_i_0_0_0_sender], [j_0_0_0_sender], [result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver, m_0_0_0_receiver, n_0_0_0_receiver, o_0_0_0_receiver, a1_0_0_0_receiver], [c_0_0_0_sender], [a_0_0_0_sender], [e_0_0_0_sender], [g_0_0_0_sender], [a1_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
|]

natPar20 = [pythonModule|
import multiprocessing as mp
def task_1(d_0_0_0_sender):
    d_0_0_0 = fun4(i)
    d_0_0_0_sender.send(d_0_0_0)
def task_2(d1_0_0_0_sender):
    d1_0_0_0 = fun4(i)
    d1_0_0_0_sender.send(d1_0_0_0)
def task_3(m_0_0_0_sender):
    m_0_0_0 = fun13(i)
    m_0_0_0_sender.send(m_0_0_0)
def task_4(o_0_0_0_sender):
    o_0_0_0 = fun15(i)
    o_0_0_0_sender.send(o_0_0_0)
def task_5(f_0_0_0_sender):
    f_0_0_0 = fun6(i)
    f_0_0_0_sender.send(f_0_0_0)
def task_6(b_0_0_0_sender):
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_7(b1_0_0_0_sender):
    b1_0_0_0 = fun2(i)
    b1_0_0_0_sender.send(b1_0_0_0)
def task_8(h_0_0_0_sender):
    h_0_0_0 = fun8(i)
    h_0_0_0_sender.send(h_0_0_0)
def task_9(l_0_0_0_sender):
    l_0_0_0 = fun12(i)
    l_0_0_0_sender.send(l_0_0_0)
def task_10(k_0_0_0_sender):
    k_0_0_0 = fun11(i)
    k_0_0_0_sender.send(k_0_0_0)
def task_11(n_0_0_0_sender):
    n_0_0_0 = fun14(i)
    n_0_0_0_sender.send(n_0_0_0)
def task_12(other_i_0_0_0_sender):
    other_i_0_0_0 = fun9(i)
    other_i_0_0_0_sender.send(other_i_0_0_0)
def task_13(j_0_0_0_sender):
    j_0_0_0 = fun10(i)
    j_0_0_0_sender.send(j_0_0_0)
def task_14(c_0_0_0_sender):
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_15(a_0_0_0_sender):
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
def task_16(result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver, m_0_0_0_receiver, n_0_0_0_receiver, o_0_0_0_receiver, a1_0_0_0_receiver, b1_0_0_0_receiver, c1_0_0_0_receiver, d1_0_0_0_receiver, e1_0_0_0_receiver):
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
        result_0_0_0 = combine(i, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11, var_12, var_13, var_14, var_15, var_16, var_17, var_18, var_19, var_20)
        result_0_0_0_sender.send(result_0_0_0)
def task_17(e_0_0_0_sender):
    e_0_0_0 = fun5(i)
    e_0_0_0_sender.send(e_0_0_0)
def task_18(g_0_0_0_sender):
    g_0_0_0 = fun7(i)
    g_0_0_0_sender.send(g_0_0_0)
def task_19(a1_0_0_0_sender):
    a1_0_0_0 = fun1(i)
    a1_0_0_0_sender.send(a1_0_0_0)
def task_20(c1_0_0_0_sender):
    c1_0_0_0 = fun3(i)
    c1_0_0_0_sender.send(c1_0_0_0)
def task_21(e1_0_0_0_sender):
    e1_0_0_0 = fun5(i)
    e1_0_0_0_sender.send(e1_0_0_0)
from helpers.library_proxy import *
def main(i_1):
    global i
    i, = i_1,
    result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
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
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9, task_10, task_11, task_12, task_13, task_14, task_15, task_16, task_17, task_18, task_19, task_20, task_21]
    channels = [[d_0_0_0_sender], [d1_0_0_0_sender], [m_0_0_0_sender], [o_0_0_0_sender], [f_0_0_0_sender], [b_0_0_0_sender], [b1_0_0_0_sender], [h_0_0_0_sender], [l_0_0_0_sender], [k_0_0_0_sender], [n_0_0_0_sender], [other_i_0_0_0_sender], [j_0_0_0_sender], [c_0_0_0_sender], [a_0_0_0_sender], [result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver, m_0_0_0_receiver, n_0_0_0_receiver, o_0_0_0_receiver, a1_0_0_0_receiver, b1_0_0_0_receiver, c1_0_0_0_receiver, d1_0_0_0_receiver, e1_0_0_0_receiver], [e_0_0_0_sender], [g_0_0_0_sender], [a1_0_0_0_sender], [c1_0_0_0_sender], [e1_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result

|]

natPar24 = [pythonModule|
import multiprocessing as mp
def task_1(d_0_0_0_sender):
    d_0_0_0 = fun4(i)
    d_0_0_0_sender.send(d_0_0_0)
def task_2(d1_0_0_0_sender):
    d1_0_0_0 = fun4(i)
    d1_0_0_0_sender.send(d1_0_0_0)
def task_3(m_0_0_0_sender):
    m_0_0_0 = fun13(i)
    m_0_0_0_sender.send(m_0_0_0)
def task_4(o_0_0_0_sender):
    o_0_0_0 = fun15(i)
    o_0_0_0_sender.send(o_0_0_0)
def task_5(f_0_0_0_sender):
    f_0_0_0 = fun6(i)
    f_0_0_0_sender.send(f_0_0_0)
def task_6(b_0_0_0_sender):
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_7(f1_0_0_0_sender):
    f1_0_0_0 = fun6(i)
    f1_0_0_0_sender.send(f1_0_0_0)
def task_8(b1_0_0_0_sender):
    b1_0_0_0 = fun2(i)
    b1_0_0_0_sender.send(b1_0_0_0)
def task_9(result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver, m_0_0_0_receiver, n_0_0_0_receiver, o_0_0_0_receiver, a1_0_0_0_receiver, b1_0_0_0_receiver, c1_0_0_0_receiver, d1_0_0_0_receiver, e1_0_0_0_receiver, f1_0_0_0_receiver, g1_0_0_0_receiver, h1_0_0_0_receiver, i1_0_0_0_receiver):
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
        result_0_0_0 = combine(i, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11, var_12, var_13, var_14, var_15, var_16, var_17, var_18, var_19, var_20, var_21, var_22, var_23, var_24)
        result_0_0_0_sender.send(result_0_0_0)
def task_10(h_0_0_0_sender):
    h_0_0_0 = fun8(i)
    h_0_0_0_sender.send(h_0_0_0)
def task_11(h1_0_0_0_sender):
    h1_0_0_0 = fun8(i)
    h1_0_0_0_sender.send(h1_0_0_0)
def task_12(l_0_0_0_sender):
    l_0_0_0 = fun12(i)
    l_0_0_0_sender.send(l_0_0_0)
def task_13(k_0_0_0_sender):
    k_0_0_0 = fun11(i)
    k_0_0_0_sender.send(k_0_0_0)
def task_14(n_0_0_0_sender):
    n_0_0_0 = fun14(i)
    n_0_0_0_sender.send(n_0_0_0)
def task_15(other_i_0_0_0_sender):
    other_i_0_0_0 = fun9(i)
    other_i_0_0_0_sender.send(other_i_0_0_0)
def task_16(j_0_0_0_sender):
    j_0_0_0 = fun10(i)
    j_0_0_0_sender.send(j_0_0_0)
def task_17(c_0_0_0_sender):
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_18(a_0_0_0_sender):
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
def task_19(e_0_0_0_sender):
    e_0_0_0 = fun5(i)
    e_0_0_0_sender.send(e_0_0_0)
def task_20(g_0_0_0_sender):
    g_0_0_0 = fun7(i)
    g_0_0_0_sender.send(g_0_0_0)
def task_21(a1_0_0_0_sender):
    a1_0_0_0 = fun1(i)
    a1_0_0_0_sender.send(a1_0_0_0)
def task_22(c1_0_0_0_sender):
    c1_0_0_0 = fun3(i)
    c1_0_0_0_sender.send(c1_0_0_0)
def task_23(g1_0_0_0_sender):
    g1_0_0_0 = fun7(i)
    g1_0_0_0_sender.send(g1_0_0_0)
def task_24(i1_0_0_0_sender):
    i1_0_0_0 = fun9(i)
    i1_0_0_0_sender.send(i1_0_0_0)
def task_25(e1_0_0_0_sender):
    e1_0_0_0 = fun5(i)
    e1_0_0_0_sender.send(e1_0_0_0)
from helpers.library_proxy import *
def main(i_1):
    global i
    i, = i_1,
    result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
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
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9, task_10, task_11, task_12, task_13, task_14, task_15, task_16, task_17, task_18, task_19, task_20, task_21, task_22, task_23, task_24, task_25]
    channels = [[d_0_0_0_sender], [d1_0_0_0_sender], [m_0_0_0_sender], [o_0_0_0_sender], [f_0_0_0_sender], [b_0_0_0_sender], [f1_0_0_0_sender], [b1_0_0_0_sender], [result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver, m_0_0_0_receiver, n_0_0_0_receiver, o_0_0_0_receiver, a1_0_0_0_receiver, b1_0_0_0_receiver, c1_0_0_0_receiver, d1_0_0_0_receiver, e1_0_0_0_receiver, f1_0_0_0_receiver, g1_0_0_0_receiver, h1_0_0_0_receiver, i1_0_0_0_receiver], [h_0_0_0_sender], [h1_0_0_0_sender], [l_0_0_0_sender], [k_0_0_0_sender], [n_0_0_0_sender], [other_i_0_0_0_sender], [j_0_0_0_sender], [c_0_0_0_sender], [a_0_0_0_sender], [e_0_0_0_sender], [g_0_0_0_sender], [a1_0_0_0_sender], [c1_0_0_0_sender], [g1_0_0_0_sender], [i1_0_0_0_sender], [e1_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
    |]

natPar28 = [pythonModule|
import multiprocessing as mp
def task_1(l1_0_0_0_sender):
    l1_0_0_0 = fun12(i)
    l1_0_0_0_sender.send(l1_0_0_0)
def task_2(d_0_0_0_sender):
    d_0_0_0 = fun4(i)
    d_0_0_0_sender.send(d_0_0_0)
def task_3(d1_0_0_0_sender):
    d1_0_0_0 = fun4(i)
    d1_0_0_0_sender.send(d1_0_0_0)
def task_4(k1_0_0_0_sender):
    k1_0_0_0 = fun11(i)
    k1_0_0_0_sender.send(k1_0_0_0)
def task_5(m_0_0_0_sender):
    m_0_0_0 = fun13(i)
    m_0_0_0_sender.send(m_0_0_0)
def task_6(o_0_0_0_sender):
    o_0_0_0 = fun15(i)
    o_0_0_0_sender.send(o_0_0_0)
def task_7(j1_0_0_0_sender):
    j1_0_0_0 = fun10(i)
    j1_0_0_0_sender.send(j1_0_0_0)
def task_8(f_0_0_0_sender):
    f_0_0_0 = fun6(i)
    f_0_0_0_sender.send(f_0_0_0)
def task_9(b_0_0_0_sender):
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_10(f1_0_0_0_sender):
    f1_0_0_0 = fun6(i)
    f1_0_0_0_sender.send(f1_0_0_0)
def task_11(b1_0_0_0_sender):
    b1_0_0_0 = fun2(i)
    b1_0_0_0_sender.send(b1_0_0_0)
def task_12(result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver, m_0_0_0_receiver, n_0_0_0_receiver, o_0_0_0_receiver, a1_0_0_0_receiver, b1_0_0_0_receiver, c1_0_0_0_receiver, d1_0_0_0_receiver, e1_0_0_0_receiver, f1_0_0_0_receiver, g1_0_0_0_receiver, h1_0_0_0_receiver, i1_0_0_0_receiver, j1_0_0_0_receiver, k1_0_0_0_receiver, l1_0_0_0_receiver, m1_0_0_0_receiver):
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
        result_0_0_0 = combine(i, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11, var_12, var_13, var_14, var_15, var_16, var_17, var_18, var_19, var_20, var_21, var_22, var_23, var_24, var_25, var_26, var_27, var_28)
        result_0_0_0_sender.send(result_0_0_0)
def task_13(h_0_0_0_sender):
    h_0_0_0 = fun8(i)
    h_0_0_0_sender.send(h_0_0_0)
def task_14(h1_0_0_0_sender):
    h1_0_0_0 = fun8(i)
    h1_0_0_0_sender.send(h1_0_0_0)
def task_15(l_0_0_0_sender):
    l_0_0_0 = fun12(i)
    l_0_0_0_sender.send(l_0_0_0)
def task_16(m1_0_0_0_sender):
    m1_0_0_0 = fun13(i)
    m1_0_0_0_sender.send(m1_0_0_0)
def task_17(k_0_0_0_sender):
    k_0_0_0 = fun11(i)
    k_0_0_0_sender.send(k_0_0_0)
def task_18(n_0_0_0_sender):
    n_0_0_0 = fun14(i)
    n_0_0_0_sender.send(n_0_0_0)
def task_19(other_i_0_0_0_sender):
    other_i_0_0_0 = fun9(i)
    other_i_0_0_0_sender.send(other_i_0_0_0)
def task_20(j_0_0_0_sender):
    j_0_0_0 = fun10(i)
    j_0_0_0_sender.send(j_0_0_0)
def task_21(c_0_0_0_sender):
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_22(a_0_0_0_sender):
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
def task_23(e_0_0_0_sender):
    e_0_0_0 = fun5(i)
    e_0_0_0_sender.send(e_0_0_0)
def task_24(g_0_0_0_sender):
    g_0_0_0 = fun7(i)
    g_0_0_0_sender.send(g_0_0_0)
def task_25(a1_0_0_0_sender):
    a1_0_0_0 = fun1(i)
    a1_0_0_0_sender.send(a1_0_0_0)
def task_26(c1_0_0_0_sender):
    c1_0_0_0 = fun3(i)
    c1_0_0_0_sender.send(c1_0_0_0)
def task_27(g1_0_0_0_sender):
    g1_0_0_0 = fun7(i)
    g1_0_0_0_sender.send(g1_0_0_0)
def task_28(i1_0_0_0_sender):
    i1_0_0_0 = fun9(i)
    i1_0_0_0_sender.send(i1_0_0_0)
def task_29(e1_0_0_0_sender):
    e1_0_0_0 = fun5(i)
    e1_0_0_0_sender.send(e1_0_0_0)
from helpers.library_proxy import *
def main(i_1):
    global i
    i, = i_1,
    result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
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
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9, task_10, task_11, task_12, task_13, task_14, task_15, task_16, task_17, task_18, task_19, task_20, task_21, task_22, task_23, task_24, task_25, task_26, task_27, task_28, task_29]
    channels = [[l1_0_0_0_sender], [d_0_0_0_sender], [d1_0_0_0_sender], [k1_0_0_0_sender], [m_0_0_0_sender], [o_0_0_0_sender], [j1_0_0_0_sender], [f_0_0_0_sender], [b_0_0_0_sender], [f1_0_0_0_sender], [b1_0_0_0_sender], [result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver, m_0_0_0_receiver, n_0_0_0_receiver, o_0_0_0_receiver, a1_0_0_0_receiver, b1_0_0_0_receiver, c1_0_0_0_receiver, d1_0_0_0_receiver, e1_0_0_0_receiver, f1_0_0_0_receiver, g1_0_0_0_receiver, h1_0_0_0_receiver, i1_0_0_0_receiver, j1_0_0_0_receiver, k1_0_0_0_receiver, l1_0_0_0_receiver, m1_0_0_0_receiver], [h_0_0_0_sender], [h1_0_0_0_sender], [l_0_0_0_sender], [m1_0_0_0_sender], [k_0_0_0_sender], [n_0_0_0_sender], [other_i_0_0_0_sender], [j_0_0_0_sender], [c_0_0_0_sender], [a_0_0_0_sender], [e_0_0_0_sender], [g_0_0_0_sender], [a1_0_0_0_sender], [c1_0_0_0_sender], [g1_0_0_0_sender], [i1_0_0_0_sender], [e1_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
    |]

natPar32 = [pythonModule|
import multiprocessing as mp
def task_1(l1_0_0_0_sender):
    l1_0_0_0 = fun12(i)
    l1_0_0_0_sender.send(l1_0_0_0)
def task_2(d_0_0_0_sender):
    d_0_0_0 = fun4(i)
    d_0_0_0_sender.send(d_0_0_0)
def task_3(d1_0_0_0_sender):
    d1_0_0_0 = fun4(i)
    d1_0_0_0_sender.send(d1_0_0_0)
def task_4(k1_0_0_0_sender):
    k1_0_0_0 = fun11(i)
    k1_0_0_0_sender.send(k1_0_0_0)
def task_5(m_0_0_0_sender):
    m_0_0_0 = fun13(i)
    m_0_0_0_sender.send(m_0_0_0)
def task_6(o_0_0_0_sender):
    o_0_0_0 = fun15(i)
    o_0_0_0_sender.send(o_0_0_0)
def task_7(j1_0_0_0_sender):
    j1_0_0_0 = fun10(i)
    j1_0_0_0_sender.send(j1_0_0_0)
def task_8(result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver, m_0_0_0_receiver, n_0_0_0_receiver, o_0_0_0_receiver, a1_0_0_0_receiver, b1_0_0_0_receiver, c1_0_0_0_receiver, d1_0_0_0_receiver, e1_0_0_0_receiver, f1_0_0_0_receiver, g1_0_0_0_receiver, h1_0_0_0_receiver, i1_0_0_0_receiver, j1_0_0_0_receiver, k1_0_0_0_receiver, l1_0_0_0_receiver, m1_0_0_0_receiver, n1_0_0_0_receiver, o1_0_0_0_receiver, a2_0_0_0_receiver, b2_0_0_0_receiver):
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
        var_31 = a2_0_0_0_receiver.recv()
        var_32 = b2_0_0_0_receiver.recv()
        result_0_0_0 = combine(i, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11, var_12, var_13, var_14, var_15, var_16, var_17, var_18, var_19, var_20, var_21, var_22, var_23, var_24, var_25, var_26, var_27, var_28, var_29, var_30, var_31, var_32)
        result_0_0_0_sender.send(result_0_0_0)
def task_9(n1_0_0_0_sender):
    n1_0_0_0 = fun14(i)
    n1_0_0_0_sender.send(n1_0_0_0)
def task_10(a2_0_0_0_sender):
    a2_0_0_0 = fun1(i)
    a2_0_0_0_sender.send(a2_0_0_0)
def task_11(f_0_0_0_sender):
    f_0_0_0 = fun6(i)
    f_0_0_0_sender.send(f_0_0_0)
def task_12(b_0_0_0_sender):
    b_0_0_0 = fun2(i)
    b_0_0_0_sender.send(b_0_0_0)
def task_13(f1_0_0_0_sender):
    f1_0_0_0 = fun6(i)
    f1_0_0_0_sender.send(f1_0_0_0)
def task_14(b1_0_0_0_sender):
    b1_0_0_0 = fun2(i)
    b1_0_0_0_sender.send(b1_0_0_0)
def task_15(h_0_0_0_sender):
    h_0_0_0 = fun8(i)
    h_0_0_0_sender.send(h_0_0_0)
def task_16(h1_0_0_0_sender):
    h1_0_0_0 = fun8(i)
    h1_0_0_0_sender.send(h1_0_0_0)
def task_17(l_0_0_0_sender):
    l_0_0_0 = fun12(i)
    l_0_0_0_sender.send(l_0_0_0)
def task_18(o1_0_0_0_sender):
    o1_0_0_0 = fun15(i)
    o1_0_0_0_sender.send(o1_0_0_0)
def task_19(m1_0_0_0_sender):
    m1_0_0_0 = fun13(i)
    m1_0_0_0_sender.send(m1_0_0_0)
def task_20(k_0_0_0_sender):
    k_0_0_0 = fun11(i)
    k_0_0_0_sender.send(k_0_0_0)
def task_21(n_0_0_0_sender):
    n_0_0_0 = fun14(i)
    n_0_0_0_sender.send(n_0_0_0)
def task_22(other_i_0_0_0_sender):
    other_i_0_0_0 = fun9(i)
    other_i_0_0_0_sender.send(other_i_0_0_0)
def task_23(j_0_0_0_sender):
    j_0_0_0 = fun10(i)
    j_0_0_0_sender.send(j_0_0_0)
def task_24(b2_0_0_0_sender):
    b2_0_0_0 = fun2(i)
    b2_0_0_0_sender.send(b2_0_0_0)
def task_25(c_0_0_0_sender):
    c_0_0_0 = fun3(i)
    c_0_0_0_sender.send(c_0_0_0)
def task_26(a_0_0_0_sender):
    a_0_0_0 = fun1(i)
    a_0_0_0_sender.send(a_0_0_0)
def task_27(e_0_0_0_sender):
    e_0_0_0 = fun5(i)
    e_0_0_0_sender.send(e_0_0_0)
def task_28(g_0_0_0_sender):
    g_0_0_0 = fun7(i)
    g_0_0_0_sender.send(g_0_0_0)
def task_29(a1_0_0_0_sender):
    a1_0_0_0 = fun1(i)
    a1_0_0_0_sender.send(a1_0_0_0)
def task_30(c1_0_0_0_sender):
    c1_0_0_0 = fun3(i)
    c1_0_0_0_sender.send(c1_0_0_0)
def task_31(g1_0_0_0_sender):
    g1_0_0_0 = fun7(i)
    g1_0_0_0_sender.send(g1_0_0_0)
def task_32(i1_0_0_0_sender):
    i1_0_0_0 = fun9(i)
    i1_0_0_0_sender.send(i1_0_0_0)
def task_33(e1_0_0_0_sender):
    e1_0_0_0 = fun5(i)
    e1_0_0_0_sender.send(e1_0_0_0)
from helpers.library_proxy import *
def main(i_1):
    global i
    i, = i_1,
    result_0_0_0_sender, result_0_0_0_receiver = mp.Pipe()
    b2_0_0_0_sender, b2_0_0_0_receiver = mp.Pipe()
    a2_0_0_0_sender, a2_0_0_0_receiver = mp.Pipe()
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
    tasks = [task_1, task_2, task_3, task_4, task_5, task_6, task_7, task_8, task_9, task_10, task_11, task_12, task_13, task_14, task_15, task_16, task_17, task_18, task_19, task_20, task_21, task_22, task_23, task_24, task_25, task_26, task_27, task_28, task_29, task_30, task_31, task_32, task_33]
    channels = [[l1_0_0_0_sender], [d_0_0_0_sender], [d1_0_0_0_sender], [k1_0_0_0_sender], [m_0_0_0_sender], [o_0_0_0_sender], [j1_0_0_0_sender], [result_0_0_0_sender, a_0_0_0_receiver, b_0_0_0_receiver, c_0_0_0_receiver, d_0_0_0_receiver, e_0_0_0_receiver, f_0_0_0_receiver, g_0_0_0_receiver, h_0_0_0_receiver, other_i_0_0_0_receiver, j_0_0_0_receiver, k_0_0_0_receiver, l_0_0_0_receiver, m_0_0_0_receiver, n_0_0_0_receiver, o_0_0_0_receiver, a1_0_0_0_receiver, b1_0_0_0_receiver, c1_0_0_0_receiver, d1_0_0_0_receiver, e1_0_0_0_receiver, f1_0_0_0_receiver, g1_0_0_0_receiver, h1_0_0_0_receiver, i1_0_0_0_receiver, j1_0_0_0_receiver, k1_0_0_0_receiver, l1_0_0_0_receiver, m1_0_0_0_receiver, n1_0_0_0_receiver, o1_0_0_0_receiver, a2_0_0_0_receiver, b2_0_0_0_receiver], [n1_0_0_0_sender], [a2_0_0_0_sender], [f_0_0_0_sender], [b_0_0_0_sender], [f1_0_0_0_sender], [b1_0_0_0_sender], [h_0_0_0_sender], [h1_0_0_0_sender], [l_0_0_0_sender], [o1_0_0_0_sender], [m1_0_0_0_sender], [k_0_0_0_sender], [n_0_0_0_sender], [other_i_0_0_0_sender], [j_0_0_0_sender], [b2_0_0_0_sender], [c_0_0_0_sender], [a_0_0_0_sender], [e_0_0_0_sender], [g_0_0_0_sender], [a1_0_0_0_sender], [c1_0_0_0_sender], [g1_0_0_0_sender], [i1_0_0_0_sender], [e1_0_0_0_sender]]
    processes = []
    for task, channels in zip(tasks, channels):
        process = mp.Process(target=task, args=channels)
        processes.append(process)
    list(map(mp.Process.start, processes))
    result = result_0_0_0_receiver.recv()
    list(map(mp.Process.terminate, processes))
    list(map(mp.Process.join, processes))
    return result
    |]