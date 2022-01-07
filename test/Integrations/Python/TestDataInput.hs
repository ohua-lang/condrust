{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.TestDataInput where

import Integrations.Python.SimpleQuoter


testLib = [pythonModule|
from typing import List

def hello_world():
    return "Hello World"

def check(v):
   return v < 10

def double(x):
    return x, x


def funInt():
    return 42

def f(arg = None):
    if arg:
        return arg + 2
    return 7

def f1(x):
    return x+2

def f2(x):
    return x+3

def g(arg= None):
    return 4

def g0(x):
    return x+7

def g1(x):
    return x+8

def h(x = None):
    return 23

def oneArg(x):
    return x

def listOfMObs(a):
    mobs = [MObs(i) for i in range(a)]
    return mobs

def moreArgs(x,y,z):
    return x+y

def changeRef(x:int):
    x = x+23

def changeRefType(x:int):
    x = "Not an int anymore"

def mutateValues(intList:List[int]):
    intList = [i+1 for i in intList]

def mutateType(intList:List[int]):
    intList = tuple(intList)


def some_invented_iter_function():
    return list(range(0, 11))

class MObs(object):
    def __init__(self, num):
        self.num = num
    def addNum(self, num):
        self.num += num
    def getNum(self):
        return self.num
    def clone(self):
        return MObs(self.getNum())

def mobFun(mob, num):
    mob.addNum(num)
    return mob.getNum()
|]

-- Test cases for Basic.hs ------------------------------
callAFunction = [pythonModule|
from testLib import *

def algo():
    hello_world()
|]

exprArg = [pythonModule|
from testLib import *

def algo():
    x = oneArg((lambda x : x+2)(4))
    return x
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
    return x
|]

nestedCompose = [pythonModule|
from testLib import *

def algo():
    x = moreArgs(funInt(), oneArg(funInt()), 42)
    return x
|]

tupleArgumentCall= [pythonModule|
from testLib import *

def algo(a,b):
    tpl = (a,b)
    x = oneArg(tpl)
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

assignEmptyList = [pythonModule|
from testLib import *

def algo(a, b):
    x = []
    return x
|]

assignList = [pythonModule|
from testLib import *

def algo(a, b):
    x = [a,b]
    return x
|]


assignEmptyDict = [pythonModule|
from testLib import *

def algo(a, b):
    x = {}
    return x
|]

assignDict = [pythonModule|
from testLib import *

def algo(a, b):
    x = {a:1, b:2}
    return x
|]


assignSet = [pythonModule|
from testLib import *

def algo(a, b):
    x = {a,b,1,2,3}
    return x
|]


assignSubscript = [pythonModule|
from testLib import *

def algo(a, b):
    l = [1,2,3]
    x = l[0]
    return x
|]

assignToSubscript = [pythonModule|
from testLib import *

def algo(a, b):
    l = [1,2,3]
    l[1] = f()
    return l
|]


assignFromDictKey = [pythonModule|
from testLib import *

def algo(a, b):
    d = {"x":2, "b":4}
    i = function_that_returns_a_string_literal_because_we_dont_support_them()
    x = d[i]
    return x
|]

assignToDictKey = [pythonModule|
from testLib import *

def algo(a, b):
    d = {"a":2, "b":4}
    i = function_that_returns_a_string_literal_because_we_dont_support_them()
    d[i] = 42
    return d
|]


assignListCompr = [pythonModule|
from testLib import *

def algo(a, b):
    l = [1,2,3]
    x = [2*i for i in l]
    return x
|]

justListCompr = [pythonModule|
from testLib import *

def algo(a, b):
    [sideEffect(i) for i in range(10)]
|]

justListComprComp = [pythonModule|
from testLib import *

def algo(a, b):
    tempList = []
    for i in range(10):
        temp = sideEffect(i)
        tempList.append(temp)
|]


--Test cases for State.hs ---------------------------------------------




--Test cases for Loops.hs ---------------------------------------------
loopIterator= [pythonModule|
from testLib import *
def algo():
    s = MObs(42)
    stream = some_invented_iter_function()
    for e in stream:
        r = h(e)
        s.addNum(r)
    return s
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
    g = {1:2, 3:4}
    mOb = MObs(42)
    for (i,j) in g.items():
        n = f(i)
        mOb.addNum(n)
    return mOb
|]

loopCommaPattern= [pythonModule|
from testLib import *

def algo(a):
    g = {1:2, 3:4}
    mOb = MObs(42)
    for i,j in g.items():
        n = f(i)
        mOb.addNum(n)
        mOb.addNum(3)
    return mOb
|]


whileLoop = [pythonModule|
from testLib import *

def algo(a):
    n = iter_32()
    s = newState()
    while n.has_next():
        e = n.next()
        s.gs(e)
    return s
|]


--Test cases for IfElse.hs --------------------------------------------
condExpr = [pythonModule|
from testLib import *

def algo(i):
    a = f()
    b = f1(2)
    c = f2(3)
    d = g0(b) if a else g1(c)
    return oneArg(d) 
|]

condExprState = [pythonModule|
from testLib import *

def algo(i):
    mob = MObs(3)
    b, c = double(i)
    d = g0(b) if mob.getNum() else g1(c)
    return oneArg(d) 
|]

condContextFunction = [pythonModule|
from testLib import *

def algo(i):
    a = f1(i)
    d = g0(5) if a else f()
    return oneArg(d) 
|]

condExprComCond = [pythonModule|
from testLib import *

def algo(i):
    a = f()
    d = True if (a < 3) else False
    return d 
|]


condExprFunCond = [pythonModule|
from testLib import *

def algo(i):
    d = g() if f() else f1(4)
    return d 
|]

condExprLit = [pythonModule|
from testLib import *

def algo(i):
    cond = f(i)
    x = 1 if cond else 0
    return x
|]

condExprStateFunRet = [pythonModule|
from testLib import *

def algo(i):
    mob1 = MObs(3)
    mob2 = MObs(23)
    b = f()
    d = mob1.getNum() if b else mob2.getNum()
    return d 
|]

iteOnlyIf = [pythonModule|
from testLib import *

def algo(i):
    a = f(i)
    b = f()
    if a:
        b = g()
    return b 
|]

iteOnlyIfState = [pythonModule|
from testLib import *

def algo(i):
    a = f(i)
    mob = MObs(7)
    if a:
        mob.addNum(16)
    return mob
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


iteLiteralArgs = [pythonModule|
from testLib import *

def algo(a): 
    b = f(a)
    if b:
        x = f(13)
    else:
        x = g(14)
    return x
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

iteStateful = [pythonModule|
from testLib import *

def algo(i):
    mob = MObs(22) 
    a = f(i)
    if a:
        mob.addNum(3)
    else:
        mob.addNum(23)
    return mob
|]

--Test cases for State.hs --------------------------------------------
callMethod = [pythonModule|
from testLib import *

def algo():
    mob = MObs(22)
    # mob.addNum(21)
    x = mob.getNum()
    return x
|]

flat= [pythonModule|
from testLib import *

def algo(i):
    mob = MObs(i)
    result = mob.getNum()
    return oneArg(result) 
|]

thread = [pythonModule|
from testLib import *

def algo(i):
    mob = MObs(i)
    mob.addNum(23)
    result = mob.getNum()
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
from testLib import *

def algo(a):
    mob = MObs(a)
    g = some_invented_iter_function()
    for e in g:
        mob.addNum(e)
    return mob.getNum()
|]

stateOut = [pythonModule|
from testLib import *

def algo(a):
    mob = MObs(a)
    mob2 = mob.clone()
    g = some_invented_iter_function()
    for e in g:
        x = mobFun(mob2, e) 
        mob.addNum(x)
    return mob
|]



--Test cases for TailRec.hs --------------------------------------------
tailRecExpr= [pythonModule|
from testLib import *
def algo2(i):
    j,k = double(i)
    recursionFlag = g0(j)
    returnV = g0(k)
    return  algo2(returnV) if check(recursionFlag) else returnV

def algo():
    return algo2(1)
|]

tailRecExpr2= [pythonModule|
from testLib import *
def algo2(a,b):
    i = g0(a)
    j = g0(b)
    k = twoArgs(i,j)
    k2 = twoArgs(i,j)
    return algo2(i, j) if check(k) else k2

def algo(a, b):
    return algo2(a, b)
|]

tailRecStmt= [pythonModule|
from testLib import *
def algo2(i):
    recursionFlag = g0(i)
    returnV = g0(i)
    if check(recursionFlag):
        return algo2(returnV)
    else: 
        return returnV 

def algo():
    return algo2(1)
|]

tailRecMultiArg= [pythonModule|
from testLib import *

def algo(one, two):
    i = g0(one)
    j = g0(two)
    k = twoArgs(i,j)
    if check(k):
        return algo(i, j)
    else:
        return k

def algo2():
    algo(2,  4)
|]

tailRecContext= [pythonModule|
from testLib import *
def algo(one):
    i = g0(one)
    j = f()
    k = twoArgs(i,j)
    if check(k):
        return algo(k)
    else:
        return k

def algo2():
    algo(2)
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
