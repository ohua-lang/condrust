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

emptyReturn = [pythonModule|
from testLib import hello_world

def algo():
    x = f()
    return
|]

varReturn = [pythonModule|
from testLib import hello_world

def algo():
    x = f()
    return x
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

