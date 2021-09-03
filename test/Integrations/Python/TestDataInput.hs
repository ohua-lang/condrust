{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.TestDataInput where

import Integrations.Python.SimpleQuoter

{-Rust test include Arc<T> type, 
 according to Rust docs: ref counted thread-safe shared reference to non-mutable
 objects on the heap...so basically every non-mutable type in python?
 Note: Type annotations are not checked by the parser/quoter so they are merely for 
 illustration, same holds for 'typing' import-}

testLib = [pythonModule|
from typing import List

def hello_world():
    return "Hello World"

def f(arg = None):
    if arg:
        return arg
    return 7

def g(arg = None):
    return 4

def funInt():
    return 42

def oneArg(x):
    return x

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
    # return x
|]

assignBinOp = [pythonModule|
from testLib import *

def algo():
    x = 42 + 23
    return x
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
ifThenAssign = [pythonModule|
from testLib import *

def algo(a): 
    b = a
    if b:
        x = f(13)
    else:
        x = g(14)
    return x
|]

branchReturn = [pythonModule|
from testLib import *

def algo(i):
    if a:
        d = g0(b)
    else:
        return
    return h(d) 
|] 

ifThenMultiVar = [pythonModule|
from testLib import *

def algo(i):
    a = f(i)
    b = f1(i)
    c = f2(i)
    if a:
        d = g0(b)
    else:
        d = g1(c)
    return h(d) 
|]

condExpr = [pythonModule|
from testLib import *

def algo(i):
    c = f2(i)
    b = id(i)
    x = f(b) if c else g(42)
|]

