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
from testLib import hello_world

def algo():
    x = f()
    y = g()
    z = h()
|]



varReturn = [pythonModule|
from testLib import hello_world

def algo():
    x = oneArg(7)
    return x
|]

onlyReturnFunCall  = [pythonModule|
from testLib import hello_world
    
def algo():
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

compTimetypeError = [pythonModule|
x = 2
y = "Not addable to ints"
z  = x+y
|]

--Test cases for 


