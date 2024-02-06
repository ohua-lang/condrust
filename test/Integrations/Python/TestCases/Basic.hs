{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Integrations.Python.TestCases.Basic where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<))

import Integrations.Python.Setup
import qualified Integrations.Python.CodeSamples.TestDataOutput as Expect
import Integrations.Python.CodeSamples.SimpleQuoter
import Language.Python.Common.AST (ModuleSpan)

spec :: Spec
spec =
    describe "Basics" $ do
        it "Simple function call" $
            (showCode "Compiled: " =<< compileCode callAFunction) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callAFunction
                compiled `shouldBe` expected)

        it "Assignment no Return" $
            (showCode "Compiled: " =<< compileCode noReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.noReturn
                compiled `shouldBe` expected)
     
        it "Only 'return' should be the same as no 'return' " $
            (showCode "Compiled: " =<< compileCode emptyReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" =<< compileCode noReturn
                compiled `shouldBe` expected)

        it "Only 'return' should be the same as 'return None' " $
            (showCode "Compiled: " =<<  compileCode emptyReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" =<< compileCode noneReturn
                compiled `shouldBe` expected)
                
        it "Assignment binary Operation of integer literals" $
            (showCode "Compiled: " =<< compileCode assignBinOp) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOp
                compiled `shouldBe` expected)
       
        it "Assignment binary Operation of boolean literals" $
            (showCode "Compiled: " =<< compileCode assignBools) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBools
                compiled `shouldBe` expected) 
        
        it "Assignment binary Operation of vars" $
            (showCode "Compiled: " =<< compileCode assignBinOpChained) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOpChained
                compiled `shouldBe` expected)

        it "Assignment and return a variable" $
            (showCode "Compiled: " =<< compileCode varReturn) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.varReturn
                compiled `shouldBe` expected)     

        it "return function call" $
            (showCode "Compiled: " =<< compileCode onlyReturnFunCall) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.onlyReturnFunCall
                compiled `shouldBe` expected) 
        it "Chained Assignment" $
            (showCode "Compiled: " =<< compileCode chainedAssignment) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.chainedAssignment
                compiled `shouldBe` expected)
        
        it "Nested function calls" $
            (showCode "Compiled: " =<< compileCode nestedCompose) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.nestedCompose
                compiled `shouldBe` expected) 

        it "Algo with params" $
            (showCode "Compiled: " =<< compileCode algoWithParams) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.algoWithParams
                compiled `shouldBe` expected) 

        it "Apply ĺambda expr" $
            (showCode "Compiled: " =<< compileCode applyLambdaExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.applyLambdaExpr
                compiled `shouldBe` expected) 
        {-FIXME: The only version this can work is, if the inputs can be fully resoved by constant propagation, otherwise we currently don't support
            sending functions. However we still need to find out why constant propagation seems broken right now
        it "Assign ĺambda expr" $
            (showCode "Compiled: " =<< compileCode assignLambdaExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignLambdaExpr
                compiled `shouldBe` expected) 
        -}
        it "Assign empty list" $
            (showCode "Compiled: " =<< compileCode assignEmptyList) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignEmptyList
                compiled `shouldBe` expected) 
        
        it "Assign list with elements" $
            (showCode "Compiled: " =<< compileCode assignList) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignList
                compiled `shouldBe` expected) 

        it "Assign empty dict" $
            (showCode "Compiled: " =<< compileCode assignEmptyDict) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignEmptyDict
                compiled `shouldBe` expected) 


        it "Assign dict with elements" $
            (showCode "Compiled: " =<< compileCode assignDict) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignDict
                compiled `shouldBe` expected) 

        it "Assign set with elements" $
            (showCode "Compiled: " =<< compileCode assignSet) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignSet
                compiled `shouldBe` expected) 
        
        it "Tuple as argument" $
            (showCode "Compiled: " =<< compileCode tupleArgumentCall) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tupleArgumentCall
                compiled `shouldBe` expected)

        it "Assign to subscript" $
            (showCode "Compiled: " =<< compileCode assignToSubscript) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignToSubscript
                compiled `shouldBe` expected)
               
        it "Assign subscript to var" $
            (showCode "Compiled: " =<< compileCode assignSubscript) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignSubscript
                compiled `shouldBe` expected)

        it "String literal" $
            (showCode "Compiled: " =<< compileCode stringLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.stringLit
                compiled `shouldBe` expected)
        
        it "Float literal" $
            (showCode "Compiled: " =<< compileCode floatLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.floatLit
                compiled `shouldBe` expected)
        
        
        {-
        -- Fails until I implement augmented assignments
        it "Assignment augmented" $
            (showCode "Compiled: " =<< compileCode assignAugmented) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignAugmented
                compiled `shouldBe` expected)
        -}
------- Test Input --------------------
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

stringLit :: ModuleSpan
stringLit = [pythonModule|
from testLib import *

def algo():
    lit = "somestring"
    stringFun(lit)
|]

floatLit :: ModuleSpan
floatLit = [pythonModule|
from testLib import *

def algo():
    lit = 13.37
    floatFun(lit)
|]