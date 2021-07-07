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

def calling():
    hello_world()
|]

simpleCompose = [pythonModule|
import testLib

def algo():
    x= funInt()
    oneArg(x)
|]

compTimetypeError = [pythonModule|
x = 2
y = "Not addable to ints"
z  = x+y
|]

--Test cases for 


