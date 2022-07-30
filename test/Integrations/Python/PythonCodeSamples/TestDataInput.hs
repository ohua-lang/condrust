{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.PythonCodeSamples.TestDataInput where

import Integrations.Python.PythonCodeSamples.SimpleQuoter
import Language.Python.Common.AST (ModuleSpan)

testLib :: ModuleSpan
testLib = [pythonModule|
from typing import List

def hello_world():
    return "Hello World"

def check(v):
   return v < 10

def double(x):
    return x, x

def triple(x):
    return x, x, x

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
callAFunction :: ModuleSpan
callAFunction = [pythonModule|
from testLib import *

def algo():
    hello_world()

|]

exprArg :: ModuleSpan
exprArg = [pythonModule|
from testLib import *

def algo():
    x = oneArg((lambda x : x+2)(4))
    return x
|]

assignNumLit :: ModuleSpan
assignNumLit = [pythonModule|
from testLib import *

def algo():
    x = 5
|]

assignNumLitReturn :: ModuleSpan
assignNumLitReturn = [pythonModule|
from testLib import *

def algo():
    x = 5
    return x
|]

assignBinOp :: ModuleSpan
assignBinOp = [pythonModule|
from testLib import *

def algo():
    x = 42 + 23
    return x
|]

assignBools :: ModuleSpan
assignBools = [pythonModule|
from testLib import *

def algo():
    a = True
    b = False
    c = a and b
    return c
|]

assignBinOpChained :: ModuleSpan
assignBinOpChained = [pythonModule|
from testLib import *

def algo():
    x = f()
    z = g()
    y = x + z
    return y
|]

assignAugmented :: ModuleSpan
assignAugmented = [pythonModule|
from testLib import *

def algo():
    x += 42
|]


noReturn :: ModuleSpan
noReturn = [pythonModule|
from testLib import *

def algo():
    x = funInt()
|]

emptyReturn :: ModuleSpan
emptyReturn = [pythonModule|
from testLib import *

def algo():
    x = funInt()
    return
|]

noneReturn :: ModuleSpan
noneReturn = [pythonModule|
from testLib import *

def algo():
    x = funInt()
    return None
|]

exprNoReturn :: ModuleSpan
exprNoReturn = [pythonModule|
from testLib import *

def algo():
    funInt()
|]

multiAssignment :: ModuleSpan
multiAssignment = [pythonModule|
from testLib import *

def algo():
    (x,y,z) = return3()
    a = twoArgs(x,y)
    res = z == a
    return res
|]

varReturn :: ModuleSpan
varReturn = [pythonModule|
from testLib import *

def algo():
    x = oneArg(7)
    return x
|]

onlyReturnFunCall :: ModuleSpan
onlyReturnFunCall  = [pythonModule|
from testLib import *
    
def algo():
    return f()
|]


chainedAssignment :: ModuleSpan
chainedAssignment = [pythonModule|
from testLib import *

def algo():
    a = f(42)
    x = f(a)
    z = g(x)
    return z
|]

assignmentCallReturn :: ModuleSpan
assignmentCallReturn = [pythonModule|
from testLib import *

def algo():
    a = f(42)
    x = f(a)
    return x
|]

nestedCompose :: ModuleSpan
nestedCompose = [pythonModule|
from testLib import *

def algo():
    x = moreArgs(funInt(), oneArg(funInt()), 42)
    return x
|]

tupleArgumentCall :: ModuleSpan
tupleArgumentCall= [pythonModule|
from testLib import *

def algo(a,b):
    tpl = (a,b)
    x = oneArg(tpl)
    return x
|]

algoWithParams :: ModuleSpan
algoWithParams = [pythonModule|
from testLib import *

def algo(a, b):
    x = f(a)
    y = g1(b)
    return x+y 
|]

applyLambdaExpr :: ModuleSpan
applyLambdaExpr = [pythonModule|
from testLib import *

def algo(a, b):
    x = (lambda a : 2*a)(3)
    return x
|] 

assignLambdaExpr :: ModuleSpan
assignLambdaExpr = [pythonModule|
from testLib import *

def algo(a, b):
    x = lambda a : 2*a
    y = x(3)
    return y
|]

assignEmptyList :: ModuleSpan
assignEmptyList = [pythonModule|
from testLib import *

def algo(a, b):
    x = []
    return x
|]

assignList :: ModuleSpan
assignList = [pythonModule|
from testLib import *

def algo(a, b):
    x = [a,b]
    return x
|]


assignEmptyDict :: ModuleSpan
assignEmptyDict = [pythonModule|
from testLib import *

def algo(a, b):
    x = {}
    return x
|]


assignDict :: ModuleSpan
assignDict = [pythonModule|
from testLib import *

def algo(a, b):
    x = {a:1, b:2}
    return x
|]

assignSet :: ModuleSpan
assignSet = [pythonModule|
from testLib import *

def algo(a, b):
    x = {a,b,1,2,3}
    return x
|]

assignSubscript :: ModuleSpan
assignSubscript = [pythonModule|
from testLib import *

def algo(a, b):
    l = [1,2,3]
    x = l[0]
    return x
|]

assignToSubscript :: ModuleSpan
assignToSubscript = [pythonModule|
from testLib import *

def algo(a, b):
    l = [1,2,3]
    l[1] = f()
    return l
|]

assignFromDictKey :: ModuleSpan
assignFromDictKey = [pythonModule|
from testLib import *

def algo(a, b):
    d = {"x":2, "b":4}
    i = function_that_returns_a_string_literal_because_we_dont_support_them()
    x = d[i]
    return x
|]

assignToDictKey :: ModuleSpan
assignToDictKey = [pythonModule|
from testLib import *

def algo(a, b):
    d = {"a":2, "b":4}
    i = function_that_returns_a_string_literal_because_we_dont_support_them()
    d[i] = 42
    return d
|]

assignListCompr :: ModuleSpan
assignListCompr = [pythonModule|
from testLib import *

def algo(a, b):
    l = [1,2,3]
    x = [2*i for i in l]
    return x
|]

justListCompr :: ModuleSpan
justListCompr = [pythonModule|
from testLib import *

def algo(a, b):
    [sideEffect(i) for i in range(10)]
|]

justListComprComp :: ModuleSpan
justListComprComp = [pythonModule|
from testLib import *

def algo(a, b):
    tempList = []
    for i in range(10):
        temp = sideEffect(i)
        tempList.append(temp)
|]


--Test cases for State.hs ---------------------------------------------
argsAndKwargs :: ModuleSpan
argsAndKwargs = [pythonModule|
from testLib import *


def algo(a:str= "hihi", b:int=0):
    x = f(a,b)
    return x
|]


--Test cases for Loops.hs ---------------------------------------------
loopIterator :: ModuleSpan 
loopIterator = [pythonModule|
from testLib import *
def algo():
    s = MObs(42)
    stream = some_invented_iter_function()
    for e in stream:
        r = h(e)
        s.addNum(r)
    return s
|]

loopIterObj :: ModuleSpan
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

loop3 :: ModuleSpan
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

loopTuplePattern :: ModuleSpan
loopTuplePattern = [pythonModule|
from testLib import *
def algo(a):
    g = {1:2, 3:4}
    mOb = MObs(42)
    for (i,j) in g.items():
        n = f(i)
        mOb.addNum(n)
    return mOb
|]

loopCommaPattern :: ModuleSpan
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

whileLoop :: ModuleSpan
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
condExpr :: ModuleSpan
condExpr = [pythonModule|
from testLib import *

def algo(i):
    a = f()
    b = f1(2)
    c = f2(3)
    d = g0(b) if a else g1(c)
    return oneArg(d) 
|]

condExprState :: ModuleSpan
condExprState = [pythonModule|
from testLib import *

def algo(i):
    mob = MObs(3)
    b, c = double(i)
    d = g0(b) if mob.getNum() else g1(c)
    return oneArg(d) 
|]

condContextFunction :: ModuleSpan
condContextFunction = [pythonModule|
from testLib import *

def algo(i):
    a = f1(i)
    d = g0(5) if a else f()
    return oneArg(d) 
|]

condExprComCond :: ModuleSpan
condExprComCond = [pythonModule|
from testLib import *

def algo(i):
    a = f()
    d = True if (a < 3) else False
    return d 
|]

condExprFunCond :: ModuleSpan
condExprFunCond = [pythonModule|
from testLib import *

def algo(i):
    d = g() if f() else f1(4)
    return d 
|]

condExprLit :: ModuleSpan
condExprLit = [pythonModule|
from testLib import *

def algo(i):
    cond = f(i)
    x = 1 if cond else 0
    return x
|]

condExprStateFunRet :: ModuleSpan
condExprStateFunRet = [pythonModule|
from testLib import *

def algo(i):
    mob1 = MObs(3)
    mob2 = MObs(23)
    b = f()
    d = mob1.getNum() if b else mob2.getNum()
    return d 
|]

iteOnlyIf :: ModuleSpan
iteOnlyIf = [pythonModule|
from testLib import *

def algo(i):
    a = f(i)
    b = f()
    if a:
        b = g()
    return b 
|]

iteOnlyIfState :: ModuleSpan
iteOnlyIfState = [pythonModule|
from testLib import *

def algo(i):
    a = f(i)
    mob = MObs(7)
    if a:
        mob.addNum(16)
    return mob
|]

branchReturn :: ModuleSpan
branchReturn = [pythonModule|
from testLib import *

def algo(a,b):
    if a:
        d = g0(b)
    else:
        return
    return oneArg(d) 
|] 

iteLiteralArgs :: ModuleSpan
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

iteRustExample :: ModuleSpan
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

iteStateful :: ModuleSpan
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
callMethod :: ModuleSpan
callMethod = [pythonModule|
from testLib import *

def algo():
    mob = MObs(22)
    # mob.addNum(21)
    x = mob.getNum()
    return x
|]

flat :: ModuleSpan
flat = [pythonModule|
from testLib import *

def algo(i):
    mob = MObs(i)
    result = mob.getNum()
    return oneArg(result) 
|]

thread :: ModuleSpan
thread = [pythonModule|
from testLib import *

def algo(i):
    mob = MObs(i)
    mob.addNum(23)
    result = mob.getNum()
    return result 
|]

loop :: ModuleSpan
loop = [pythonModule|
from testLib import *

def algo(a):
    g = listOfMObs(a)
    for e in g:
        e.addNum(7)
|]

singleIO :: ModuleSpan
singleIO = [pythonModule|
from testLib import *

def algo(a):
    mob = MObs(a)
    g = some_invented_iter_function()
    for e in g:
        mob.addNum(e)
|]

singleState :: ModuleSpan
singleState = [pythonModule|
from testLib import *

def algo(a):
    mob = MObs(a)
    g = some_invented_iter_function()
    for e in g:
        mob.addNum(e)
    return mob.getNum()
|]

stateOut :: ModuleSpan
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

nested :: ModuleSpan
nested = [pythonModule|
from testLib import *

def algo(n):
  stream = Stream(n)
  store = Store()
  for worklist in stream:
    for item in worklist:
      update = handle_message(store.clone(), item)
      store.update(res)
|]


--Test cases for TailRec.hs --------------------------------------------
tailRecExpr :: ModuleSpan
tailRecExpr = [pythonModule|
from testLib import *
def algo2(i):
    j,k = double(i)
    recursionFlag = g0(j)
    returnV = g0(k)
    return  algo2(returnV) if check(recursionFlag) else returnV

def algo():
    return algo2(1)
|]

tailRecExpr2 :: ModuleSpan
tailRecExpr2 = [pythonModule|
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

tailRecStmt :: ModuleSpan
tailRecStmt = [pythonModule|
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

tailRecMultiArg :: ModuleSpan
tailRecMultiArg = [pythonModule|
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

tailRecContext :: ModuleSpan
tailRecContext = [pythonModule|
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


twoFuns :: ModuleSpan
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
twoAlgos :: ModuleSpan
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

testMod :: ModuleSpan
testMod = [pythonModule|
# from testLib import *
print("Called test module")
|]

globalRef :: ModuleSpan
globalRef = [pythonModule|
from testLib import *
GLOB = 42

def algo():
    x = fun(GLOB)
|]