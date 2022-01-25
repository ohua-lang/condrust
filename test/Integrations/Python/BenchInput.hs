{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.BenchInput where

import Integrations.Python.SimpleQuoter

blackScholes = [pythonModule|
from bs_lib import calulateForOption, refl

def algo(ops):
    valOps = refl(ops)
    results = list()
    for op in valOps:
        n = calulateForOption(op)
        results.append(n)
    return results
|]

nBodies = [pythonModule|
from lib import *

def algo(nIterations):
    solarSystem = newSystem()
    energy_states = []
    for i in range(0,nIterations):
        energy = report_energy(solarSystem)
        energy_states.append(energy)
        solarSystem.advance()
    return energy_states
|]

natPar4 = [pythonModule|
from helpers.library_proxy import *

def algo(i):
    a = fun1(i)
    b = fun2(i)
    c = fun3(i)
    d = fun4(i)
    result = combine(i,a,b,c,d)
    return result
|]

natPar8 = [pythonModule|
from helpers.library_proxy import *

def algo(i):
    a = fun1(i)
    b = fun2(i)
    c = fun3(i)
    d = fun4(i)
    e = fun5(i)
    f = fun6(i)
    g = fun7(i)
    h = fun8(i)
    result = combine(i, a,b,c,d,e,f,g,h)
    return result
|]


natPar12 = [pythonModule|
from helpers.library_proxy import *

def algo(i):
    a = fun1(i)
    b = fun2(i)
    c = fun3(i)
    d = fun4(i)
    e = fun5(i)
    f = fun6(i)
    g = fun7(i)
    h = fun8(i)
    other_i = fun9(i)
    j = fun10(i)
    k = fun11(i)
    l = fun12(i)
    result = combine(i,a,b,c,d,e,f,g, h, other_i, j, k, l)
    return result
|]


natPar16 = [pythonModule|
from helpers.library_proxy import *

def algo(i):
    a = fun1(i)
    b = fun2(i)
    c = fun3(i)
    d = fun4(i)
    e = fun5(i)
    f = fun6(i)
    g = fun7(i)
    h = fun8(i)
    other_i = fun9(i)
    j = fun10(i)
    k = fun11(i)
    l = fun12(i)
    m = fun13(i)
    n = fun14(i)
    o = fun15(i)
    a1 = fun1(i)
    result = combine(i,a,b,c,d,e,f,g, h, other_i, j, k, l, m, n, o, a1)
    return result
|]

natPar20 = [pythonModule|
from helpers.library_proxy import *

def algo(i):
    a = fun1(i)
    b = fun2(i)
    c = fun3(i)
    d = fun4(i)
    e = fun5(i)
    f = fun6(i)
    g = fun7(i)
    h = fun8(i)
    other_i = fun9(i)
    j = fun10(i)
    k = fun11(i)
    l = fun12(i)
    m = fun13(i)
    n = fun14(i)
    o = fun15(i)
    a1 = fun1(i)
    b1 = fun2(i)
    c1 = fun3(i)
    d1 = fun4(i)
    e1 = fun5(i)
    result = combine(i,a,b,c,d,e,f,g, h, other_i, j, k, l, m, n, o, a1,b1,c1,d1,e1)
    return result
|]


natPar24 = [pythonModule|
from helpers.library_proxy import *

def algo(i):
    a = fun1(i)
    b = fun2(i)
    c = fun3(i)
    d = fun4(i)
    e = fun5(i)
    f = fun6(i)
    g = fun7(i)
    h = fun8(i)
    other_i = fun9(i)
    j = fun10(i)
    k = fun11(i)
    l = fun12(i)
    m = fun13(i)
    n = fun14(i)
    o = fun15(i)
    a1 = fun1(i)
    b1 = fun2(i)
    c1 = fun3(i)
    d1 = fun4(i)
    e1 = fun5(i)
    f1 = fun6(i)
    g1 = fun7(i)
    h1 = fun8(i)
    i1 = fun9(i)
    result = combine(i, a,b,c,d,e,f,g, h, other_i, j, k, l, m, n, o, a1,b1,c1,d1,e1,f1,g1, h1, i1)
    return result
|]


natPar28 = [pythonModule|
from helpers.library_proxy import *

def algo(i):
    a = fun1(i)
    b = fun2(i)
    c = fun3(i)
    d = fun4(i)
    e = fun5(i)
    f = fun6(i)
    g = fun7(i)
    h = fun8(i)
    other_i = fun9(i)
    j = fun10(i)
    k = fun11(i)
    l = fun12(i)
    m = fun13(i)
    n = fun14(i)
    o = fun15(i)
    a1 = fun1(i)
    b1 = fun2(i)
    c1 = fun3(i)
    d1 = fun4(i)
    e1 = fun5(i)
    f1 = fun6(i)
    g1 = fun7(i)
    h1 = fun8(i)
    i1 = fun9(i)
    j1 = fun10(i)
    k1 = fun11(i)
    l1 = fun12(i)
    m1 = fun13(i)
    result = combine(i, a,b,c,d,e,f,g, h, other_i, j, k, l, m, n, o, a1,b1,c1,d1,e1,f1,g1, h1, i1, j1, k1, l1, m1)
    return result
|]


natPar32 = [pythonModule|
from helpers.library_proxy import *

def algo(i):
    a = fun1(i)
    b = fun2(i)
    c = fun3(i)
    d = fun4(i)
    e = fun5(i)
    f = fun6(i)
    g = fun7(i)
    h = fun8(i)
    other_i = fun9(i)
    j = fun10(i)
    k = fun11(i)
    l = fun12(i)
    m = fun13(i)
    n = fun14(i)
    o = fun15(i)
    a1 = fun1(i)
    b1 = fun2(i)
    c1 = fun3(i)
    d1 = fun4(i)
    e1 = fun5(i)
    f1 = fun6(i)
    g1 = fun7(i)
    h1 = fun8(i)
    i1 = fun9(i)
    j1 = fun10(i)
    k1 = fun11(i)
    l1 = fun12(i)
    m1 = fun13(i)
    n1 = fun14(i)
    o1 = fun15(i)
    a2 = fun1(i)
    b2 = fun2(i)
    result = combine(i, a,b,c,d,e,f,g, h, other_i, j, k, l, m, n, o, a1,b1,c1,d1,e1,f1,g1, h1, i1, j1, k1, l1, m1, n1, o1, a2, b2)
    return result
|]


loop3 = [pythonModule|
from helpers.library_proxy import *


def algo(i):
    result = []
    for j in range(0, i):
        x = fun1(j)
        y = fun2(x)
        z = fun3(y)
        result.append(z)
    return result
|]

ifElse =  [pythonModule|
from helpers.library_proxy import *

def algo(i):
    d = fun1(i) if check(i) else fun2(i)
    return d 
|]

-- Interesting Error: Variables 'lit_unit_0_1' and 'lit_unit_0_2' are "invented" including according channels
-- Those variables are/should be received before fun1()/fun2() are called. 
-- However they are never calculated or send anywhere 
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

--Error: unsupported output configuration 
ifElseLoop''' =  [pythonModule|
from helpers.library_proxy import *


def algo(i):
    result = []
    for j in range(i):
        x,y,z = triple(j)
        d = fun1(x) if check(z) else fun2(y)
        result.append(d)
    return result 
|]

-- Very interessting Error: Channels 'obj_0_0_2_3_sender' and 'obj_0_0_2_4_sender' are used (and obvioulsy 
-- also appear in the channels per task) but not intialized. This means they are present in channels per task
-- but not in the channels list of the whole program
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


--Error: we currently do not support destructuring and dispatch for loop data
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


-- [Error] Unsupported multiple outputs for state on stateful function
ifElseLoopStates =  [pythonModule|
from helpers.library_proxy import *


def algo(statefulObjs):
    result = []
    for obj in statefulObjs:
        d = obj.method() if obj.check() else obj.otherMethod()
        result.append(d)
    return result 
|]

--[Error] Compiler invariant broken! Input to SMap must be var not literal:
ifElseLoopStates' =  [pythonModule|
from helpers.library_proxy import *


def algo(statefulObjs):
    result = []
    for obj in statefulObjs:
        var = obj.method()
        d = fun1(var) if check(var) else fun2(var)
        result.append(d)
    return result 
|]

ifElseLoopStates'' =  [pythonModule|
from helpers.library_proxy import *


def algo(statefulObjs):
    result = []
    for obj in list(statefulObjs):
        var = obj.method()
        d = fun1(var) if check(var) else fun2(var)
        result.append(d)
    return result 
|]

-- Python Error because channel is used in two tasks
ifElseLoopStateVar =  [pythonModule|
from helpers.library_proxy import *


def algo(i):
    result = []
    for j in range(i):
        obj = fun1(j)
        d = obj.method() if check(j) else obj.otherMethod()
        result.append(d)
    return result 
|]


-- Error: We currently do not support destructuring and dispatch for loop data
ifElseLoopTwoFuns =  [pythonModule|
from helpers.library_proxy import *

def inner(i):
    d = fun1(i) if check(i) else fun2(i)
    return d

def algo(i):
    result = []
    for j in range(i):
        d = inner(j)
        result.append(d)
    return result
|]

--Error: unssupported output configuration
ifElseLoopThree =  [pythonModule|
from helpers.library_proxy import *

def algo(i):
    result = []
    for j in range(i):
        c1, c2 = double(j)
        d = fun1(c2) if check(c1) else fun2(c2)
        result.append(d)
    return result
|]


--Error: Doesn't work because recursion is  in the else branche
tailRec =  [pythonModule|
from helpers.library_proxy import *

def rec(i):
    current = fun1(i)
    c1, c2 = double(current)
    return c1 if check(current) else rec(c2)

def algo(i):
    result = rec(i)
    return result 
|]

--Error: Host expression encountered ... 'This is a compiler error please report'
-- The problem is, that I must not call the recursive fundtion directly with parameter 'i'
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


tailRecWorksRet =  [pythonModule|
from testlib import *

def rec(i):
    c1, c2 = double(i)
    flag = g0(c1)
    current = g0(c2)
    return rec(current) if check(flag) else current

def algo():
    result = rec(7)
    return result
|]

tailRecWorks =  [pythonModule|
from testlib import *

def rec(i):
    c1, c2 = double(i)
    flag = g0(c1)
    current = g0(c2)
    return rec(current) if check(flag) else current

def algo(i):
    a = f(i)
    return rec(a)
|]


-- Error because some 'ohua.lang.id(current)' made it into thhe output code
ifElseLikeTailRec=  [pythonModule|
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

-- It works :-D
ifElseLikeTailRec'=  [pythonModule|
from helpers.library_proxy import *

def inner(i):
    c1, c2 = double(i)
    flag = fun1(c1)
    current = fun1(c2)
    return fun2(current) if check(flag) else fun3(current)

def algo(i):
    result = []
    for j in range(i):
        d = inner(j)
        result.append(d)
    return result
|]

-- Error: unsupported output configuration
ifElseLikeTailRec''=  [pythonModule|
from helpers.library_proxy import *

def inner(i):
    c = fun1(i)
    flag, current  = double(c)
    return fun2(current) if check(flag) else fun3(current)

def algo(i):
    result = []
    for j in range(i):
        d = inner(j)
        result.append(d)
    return result
|]

ifElseLikeTailRec'''=  [pythonModule|
from helpers.library_proxy import *

def algo(i):
    result = []
    for j in range(i):
        c1, c2 = double(j)
        flag = fun1(c1)
        current = fun1(c2)
        d = fun2(current) if check(flag) else fun3(current)
        result.append(d)
    return result
|]