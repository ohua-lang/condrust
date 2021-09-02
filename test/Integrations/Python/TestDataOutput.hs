{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.TestDataOutput where

import Integrations.Python.SimpleQuoter


-- Test cases for Basic.hs
-- Test cases for Basic.hs
callAFunction = [pythonModule|
from testLib import hello_world

def algo():
    hello_world()
|]

assignNumLit = [pythonModule|
from testLib import hello_world

def algo():
    x = 5
|]

assignBinOp = [pythonModule|
from testLib import hello_world

def algo():
    y = y + 2
    x = 42 + 23
|]

assignAugmented = [pythonModule|
from testLib import hello_world

def algo():
    x += 42
|]

noReturn = [pythonModule|
from testLib import hello_world

def algo():
    x = f()
|]

multiAssignment = [pythonModule|
from testLib import hello_world

def algo():
    x = f()
    y = g()
    z = h()
|]

emptyReturn = [pythonModule|
from testLib import hello_world

def algo():
    x = f()
    return
|]

noneReturn = [pythonModule|
from testLib import hello_world

def algo():
    x = f()
    return None
|]

exprNoReturn = [pythonModule|
from testLib import *

def algo():
    funInt()
|] 

varReturn = [pythonModule|
from testLib import *

def algo():
    # x = f()
    return f()
|]

otherVarReturn = [pythonModule|
from testLib import hello_world

def algo():
    a = f(42)
    x = f()
    z = g()
    return x
|]

onlyReturnFunCall  = [pythonModule|
from testLib import hello_world
    
def algo():
    return f()
|]


returnFunCall = [pythonModule|
from testLib import hello_world

def algo():
    x = g()
    return oneArg(42)
|]

simpleCompose = [pythonModule|
import testLib

def algo():
    x= funInt()
    return oneArg(x)
|]

nestedCompose = [pythonModule|
import testLib

def algo():
    x = moreArgs(funInt(), oneArg(funInt()), 42)
    return x
|]


--Test cases for Loops.hs ---------------------------------------------
loopIterator= [pythonModule|

def algo(a):
    g = some_invented_iter_function()
    for i in g:
        f(i)
|]

loopList= [pythonModule|

def algo(a):
    for i in [1,2,3]:
        f(i)
|]


