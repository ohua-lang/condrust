{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Integrations.Python.TestCases.WIP where

import Ohua.Prelude ( ($), Monad((>>=)), (=<<), Either(..))

import Integrations.Python.Setup
import qualified Integrations.Python.CodeSamples.TestDataOutput as Expect
import Integrations.Python.CodeSamples.SimpleQuoter
import Language.Python.Common.AST (ModuleSpan)

spec :: Spec
spec =
    describe "WIP Tests" $ do
        it "GlobRef" $
            (showCode "Compiled: " =<< compileCode globalRef) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callAFunction
                compiled `shouldBe` expected)

        {-

        it "Assignment binary Operation of integer literals" $
            (showCode "Compiled: " =<< compileCode assignBinOp) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.assignBinOp
                compiled `shouldBe` expected)
        it "Accept keyword and default args" $
            (showCode "Compiled: " =<< compileCode argsAndKwargs) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.argsAndKwargs
                compiled `shouldBe` expected)
        

        it "Two Algos" $
            (showCode "Compiled: " =<< compileCode twoAlgos) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callMethod
                compiled `shouldBe` expected)

        
        it "Conditional Expression with literals" $
            (showCode "Compiled: " =<< compileCode condExprLit) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.condExprLit
                compiled `shouldBe` expected)  


        it "Tail Recursive with If-Expr" $
            (showCode "Compiled: " =<< compileCodeWithRec tailRecExpr) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)
        
        it "Tail Recursive with If-Stmt" $
            (showCode "Compiled: " =<< compileCodeWithRec tailRecStmt) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.tailRec
                compiled `shouldBe` expected)

        {-Todo: Error : unitFun must only have one output
        it "Multiassignment comma separated" $
            (showCode "Compiled: " =<< compileCode multiAssignment) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.multiAssignment
                compiled `shouldBe` expected)
         -}
        

        it "test args" $
            (showCode "Compiled: " =<< compileCode exprArg) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.callAFunction
                compiled `shouldBe` expected)
                

    it "ForLoop over iterator" $
            (showCode "Compiled: " =<< compileCode loopIterator) >>=
            (\compiled -> do
                expected <- showCode "Expected:" Expect.loopIterator
                compiled `shouldBe` expected)
        

            -}

----- Test Inputs -----------------
globalRef :: ModuleSpan
globalRef = [pythonModule|
from testLib import *
GLOB = 42

def algo():
    x = fun(GLOB)
|]

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

argsAndKwargs :: ModuleSpan
argsAndKwargs = [pythonModule|
from testLib import *


def algo(a:str= "hihi", b:int=0):
    x = f(a,b)
    return x
|]