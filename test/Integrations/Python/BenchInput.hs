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

natPar = [pythonModule|
from lib import *

def algo(i):
    a = fun1(i)
    b = fun2(i)
    c = fun3(i)
    result = combine(i,a,b,c)
    return result
|]

natPar7 = [pythonModule|
from lib import *

def algo(i):
    a = fun1(i)
    b = fun2(i)
    c = fun3(i)
    d = fun4(i)
    e = fun5(i)
    f = fun6(i)
    g = fun7(i)
    result = combine(i, a,b,c,d,e,f,g)
    return result
|]

natPar15 = [pythonModule|
from lib import *

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
    result = combine(i,a,b,c,d,e,f,g, h, other_i, j, k, l, m, n, o)
    return result
|]

natPar31 = [pythonModule|
from lib import *

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
    result = combine(i, a,b,c,d,e,f,g, h, other_i, j, k, l, m, n, o, a1,b1,c1,d1,e1,f1,g1, h1, i1, j1, k1, l1, m1, n1, o1)
    return result
|]

loop3' = [pythonModule|
from helpers.library_proxy import *


def algo(i):
    result = []
    for j in range(0, i):
        j1, j2, j3 = triple(j)
        x = fun1(j1)
        y = fun2(j2)
        z = fun3(j3)
        result.append(combine(j, x, y, z))
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