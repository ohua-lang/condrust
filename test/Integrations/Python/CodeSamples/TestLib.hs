{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.CodeSamples.TestLib where

import Integrations.Python.CodeSamples.SimpleQuoter
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

