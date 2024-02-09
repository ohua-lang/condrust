{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Python.TestCases.TailRec where

import Ohua.Commons.Prelude ( ($), Monad((>>=)), (=<<) )
import Integrations.Python.Setup

import qualified Integrations.Python.CodeSamples.TestDataOutput as Expect
import Integrations.Python.CodeSamples.SimpleQuoter
import Language.Python.Common.AST (ModuleSpan)

spec :: Spec
spec =
    describe "TailRec" $ do

        it "ITE/Expr - simple one argument recursion" $
            (showCode "Compiled: " =<< compileCodeWithRec tailRecExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)

        it "TODO: ITE/Expr - multiple arguments recursion - This is a compiler error. Please report!" $
            compileCodeWithRec tailRecExpr2 `shouldThrow` anyException
        {-
            (showCode "Compiled: " =<< compileCodeWithRec tailRecExpr2) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)
        -}

        it "TODO: ITE/Stmt - tailrecursion in if-statements, missing 'return' support" $
            compileCodeWithRec tailRecMultiArg `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCodeWithRec tailRecMultiArg) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRecMultiArg
                compiled `shouldBe` expected)-}

        it "TODO: ITE/Stmt contexted function - tailrecursion in if-statements, missing 'return' support" $
            compileCodeWithRec tailRecContext `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCodeWithRec tailRecContext) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRecContext
                compiled `shouldBe` expected)
            -}

------- Test Innputs ----------
tailRecExpr :: ModuleSpan
tailRecExpr = [pythonModule|
from testLib import *
def algo_rec(i):
    (j,k) = double(i)
    recursionFlag = g0(j)
    returnV = g0(k)
    return  algo_rec(returnV) if check(recursionFlag) else returnV

def algo():
    return algo_rec(1)
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
