{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.TestDataInput where

import Integrations.Python.SimpleQuoter
import System.Directory.Internal.Prelude (Bool(True))

{-Rust test include Arc<T> type, 
 according to Rust docs: ref counted thread-safe shared reference to non-mutable
 objects on the heap...so basically every non-mutable type in python?
 Note: Type annotations are not checked by the parser/quoter so they are merely for 
 illustration, same holds for 'typing' import-}

testLib = [pythonModule|
from typing import List

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
    x = f()
    y = g()
    z = h()
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