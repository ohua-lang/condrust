{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.TestDataInput where

import Integrations.Python.SimpleQuoter

testLib = [pythonModule|
from typing import List
import time

def hello_world():
    return "Hello World"

def funInt():
    return 42

def f(arg = None):
    if arg:
        return arg
    return 7

def f1(x):
    return x+2

def f2(x):
    return x+3

def g(arg= None):
    return 4

def g0(x):
    return x-1

def g1(x):
    return x+8

def check(x):
    return True if x>0 else False

def oneArg(x):
    return x

def twoArgs(x,y):
    return x + y

def moreArgs(x,y,z):
    return x+y

def changeRef(x:int):
    x = x+23

def changeRefType(x:int):
    x = "Not an int anymore"

def return3():
    return 1,2,3

def mutateValues(intList:List[int]):
    intList = [i+1 for i in intList]

def mutateType(intList:List[int]):
    intList = tuple(intList)

def some_invented_iter_function():
    return list(range(0, 11))

def takeAWhile(n):
    time.sleep(5)
    return n

class MObs(object):
    def __init__(self, num):
        self.num = num
    def addNum(self, num):
        self.num += num
    def getNum(self):
        return self.num
|]

-- Test cases for Basic.hs ------------------------------
callAFunction = [pythonModule|
from testLib import *

def algo():
    hello_world()
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
from testLib import *

def algo():
    x = 42 + 23
    return x
|]

assignBools = [pythonModule|
from testLib import *

def algo():
    a = True
    b = False
    c = a and b
    return c
|]

assignBinOpChained = [pythonModule|
from testLib import *

def algo():
    x = f()
    z = g()
    y = x + z
    return y
|]

assignAugmented = [pythonModule|
from testLib import *

def algo():
    x += 42
|]


noReturn = [pythonModule|
from testLib import *

def algo():
    x = funInt()
|]

emptyReturn = [pythonModule|
from testLib import *

def algo():
    x = funInt()
    return
|]

noneReturn = [pythonModule|
from testLib import *

def algo():
    x = funInt()
    return None
|]

exprNoReturn = [pythonModule|
from testLib import *

def algo():
    funInt()
|]

multiAssignment = [pythonModule|
from testLib import *

def algo():
    (x,y,z) = return3()
    a = twoArgs(x,y)
    res = z == a
    return res
|]

varReturn = [pythonModule|
from testLib import *

def algo():
    x = oneArg(7)
    return x
|]

onlyReturnFunCall  = [pythonModule|
from testLib import *
    
def algo():
    return f()
|]


chainedAssignment = [pythonModule|
from testLib import *

def algo():
    a = f(42)
    x = f(a)
    z = g(x)
    return z
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
from testLib import *

def algo():
    x = moreArgs(funInt(), oneArg(funInt()), 42)
    return x
|]

algoWithParams = [pythonModule|
from testLib import *

def algo(a, b):
    x = f(a)
    y = g1(b)
    return x+y 
|]

applyLambdaExpr = [pythonModule|
from testLib import *

def algo(a, b):
    x = (lambda a : 2*a)(3)
    return x
|] 
assignLambdaExpr = [pythonModule|
from testLib import *

def algo(a, b):
    x = lambda a : 2*a
    y = x(3)
    return y
|]

--Test cases for State.hs ---------------------------------------------
callMethod = [pythonModule|
from testLib import *

def algo():
    mob = MObs(22)
    # mob.addNum(21)
    x = mob.getNum()
    return x
|]



--Test cases for Loops.hs ---------------------------------------------
loopIterator= [pythonModule|
from testLib import *
def algo(a):
    g = some_invented_iter_function()
    mOb = MObs(42)
    for i in g:
        n = f(i)
        mOb.addNum(n)
    return mOb
|]

loopIterObj= [pythonModule|
from testLib import *
def algo(a):
    g = dict()
    mOb = MObs(42)
    for i in g.values():
        n = f(i)
        mOb.addNum(n)
    return mOb
|]

loopTuplePattern= [pythonModule|
from testLib import *
def algo(a):
    g = dict()
    mOb = MObs(42)
    for (i,j) in g.items():
        n = f(i)
        mOb.addNum(n)
    return mOb
|]

loopCommaPattern= [pythonModule|
from testLib import *
def algo(a):
    g = dict()
    mOb = MObs(42)
    for i,j in g.items():
        n = f(i)
        mOb.addNum(n)
    return mOb
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

def algo(a,b):
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

iteParam = [pythonModule|
from testLib import *

def algo(a):
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
    c = f2(i)
    b = id(i)
    x = f(b) if c else g(42)
|]


condExpr2 = [pythonModule|
from testLib import *

def algo(i):
    a = f()
    b = f1(2)
    c = f2(3)
    d = g0(b) if a else g1(c)
    return oneArg(d) 
|]

--Test cases for State.hs --------------------------------------------


--Test cases for TailRec.hs --------------------------------------------
tailRec= [pythonModule|
from testLib import *
def rec(i):
    j = g0(i)
    if check(j):
        return rec(j)
    else:
        return j

def algo():
    rec(2)
|]

tailRecMultiArg= [pythonModule|
from testLib import *
def rec(one, two):
    i = g0(one)
    j = g0(two)
    k = twoArgs(i,j)
    if check(k):
        return rec(i, j)
    else:
        return k

def algo():
    rec(2,  4)
|]

tailRecContext= [pythonModule|
from testLib import *
def rec(one):
    i = g0(one)
    j = f()
    k = twoArgs(i,j)
    if check(k):
        return rec(k)
    else:
        return k

def algo():
    rec(2)
|]

----------------
primeSums= [pythonModule|
import math

def isprime(n):
    """Returns True if n is prime and False otherwise"""
    # if not isinstance(n, int):
    #     raise TypeError("argument passed to is_prime is not of 'int' type")
    if n < 2:
        return False
    if n == 2:
        return True
    max = int(math.ceil(math.sqrt(n)))
    i = 2
    while i <= max:
        if n % i == 0:
            return False
        i += 1
    return True

def sum_primes(n):
    xs = list()
    for x in range (2,n):
        if isprime(x):
            xs.append(x)
    return sum(xs)
|]

twoFuns= [pythonModule|
from testLib import *

def algo1(a):
    x = takeAWhile(a)
    y = x+2
    return y

def algo2(a):
    xs = list()
    for x in range(1, a):
        y = algo1(x)
        xs.append(y)
    return xs
|]

-----
twoAlgos = [pythonModule|
from testLib import *

def algo():
    mob = MObs(22)
    # mob.addNum(21)
    x = mob.getNum()
    return x

def algo2():
    x = 5
    y = algo1() 
    return x + y
|]
