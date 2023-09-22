{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Integrations.Python.TestCases.IfElse where


import Ohua.Prelude ( ($), Monad((>>=)), (=<<))

import Integrations.Python.Setup
import qualified Integrations.Python.CodeSamples.TestDataOutput as Expect
import Integrations.Python.CodeSamples.SimpleQuoter
import Language.Python.Common.AST (ModuleSpan)

spec :: Spec
spec =
    describe "IfElse Expressions/Statements" $ do

        it "Ite/Expr simple stateless condition" $
            (showCode "Compiled: " =<< compileCode condExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 
        
        it "Ite/Expr stateful condition" $
            (showCode "Compiled: " =<< compileCode condExprState) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprState
                compiled `shouldBe` expected) 

        it "Ite/Expr call as condition" $
            (showCode "Compiled: " =<< compileCode condExprFunCond) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprFunCond
                compiled `shouldBe` expected)  
        
        it "Ite/Expr context function" $
            (showCode "Compiled: " =<< compileCode condContextFunction) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condContextFunction
                compiled `shouldBe` expected)
        
        it "Ite/Expr int literal branches" $
            (showCode "Compiled: " =<< compileCode condExprLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprLit
                compiled `shouldBe` expected)
        
        it "Ite/Expr state-function return values " $
            (showCode "Compiled: " =<< compileCode condExprStateFunRet) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprStateFunRet
                compiled `shouldBe` expected) 


        it "Ite/Expr bool literal branches" $
            (showCode "Compiled: " =<< compileCode condExprComCond) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprComCond
                compiled `shouldBe` expected)

        it "Ite/Stmt - Exception on branch 'return'" $
        -- Todo: Replace as soon as we have a real 'unsuported Error'
            compileCode branchReturn `shouldThrow` anyException


        it "ERROR: Ite/Stmt - Literal Args - Assignments in branches are ignored, so return is not defined" $
            compileCode iteLiteralArgs `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCode iteLiteralArgs) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteLiteralArgs
                compiled `shouldBe` expected)
            -}
       
        it "Ite/Stmt- Stateful function" $
            (showCode "Compiled: " =<< compileCode iteStateful) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.iteStateful
                compiled `shouldBe` expected)  

        it "ERROR: Ite/Stmt -  only if stateless" $
            compileCode iteOnlyIf `shouldThrow` anyException
            {-
            (showCode "Compiled: " =<< compileCode iteOnlyIf) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 
            -}

        it "ERROR; Ite/Stmt -  only if stateful" $
            compileCode iteOnlyIf `shouldThrow` anyException
        {-
            (showCode "Compiled: " =<< compileCode iteOnlyIfState) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExpr
                compiled `shouldBe` expected) 
        -}

------ Test Inputs -------------------
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
