{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Python.Backend.Convert
Description : Converts supported backend subset to elements of the python AST

This module defines the mapping from the supported backend subset of Python to the 
elements of the Python AST as defined by language-python
-}
module Ohua.Integration.Python.Backend.Convert where

import Ohua.Prelude

import Ohua.Integration.Python.Util
import qualified Ohua.Integration.Python.Backend.Subset as Sub

import Language.Python.Common (SrcSpan)
import qualified Language.Python.Common.AST as Py




subToSuite:: Sub.Suite -> Py.Suite SrcSpan
subToSuite = undefined

subToStmt:: Sub.Stmt -> Py.Statement SrcSpan 
subToStmt = undefined

subToExpr:: Sub.Expr -> Py.Expr SrcSpan 
subToExpr (Sub.Int i) = Py.Int{int_value=i, expr_literal=show i, expr_annot= noSpan}
subToExpr (Sub.Bool b) = Py.Bool{bool_value= b, expr_annot = noSpan}
subToExpr Sub.None = Py.None noSpan 
subToExpr (Sub.Var bnd) = toPyVar . fromBinding $ bnd
-- subToExpr (Sub.Call )







subToBinOp:: Sub.BinOp -> Py.Op SrcSpan 
subToBinOp Sub.Plus = Py.Plus noSpan
subToBinOp Sub.Minus = Py.Minus noSpan 
subToBinOp Sub.Multiply = Py.Multiply noSpan 
subToBinOp Sub.Divide = Py.Divide noSpan 
subToBinOp Sub.FloorDivide = Py.FloorDivide noSpan 
subToBinOp Sub.Modulo = Py.Modulo noSpan 
subToBinOp Sub.Exponent = Py.Exponent noSpan 
subToBinOp Sub.MatrixMult = Py.MatrixMult noSpan 

subToBinOp Sub.Or  = Py.Or noSpan 
subToBinOp Sub.In = Py.In noSpan 
subToBinOp Sub.Is = Py.Is noSpan 
subToBinOp Sub.IsNot = Py.IsNot noSpan 
subToBinOp Sub.NotIn = Py.NotIn noSpan 

subToBinOp Sub.LessThan = Py.LessThan noSpan 
subToBinOp Sub.GreaterThan = Py.GreaterThan noSpan 
subToBinOp Sub.Equality  = Py.Equality noSpan 
subToBinOp Sub.GreaterThanEquals = Py.GreaterThanEquals noSpan 
subToBinOp Sub.LessThanEquals = Py.LessThanEquals noSpan 
subToBinOp Sub.NotEquals = Py.NotEquals noSpan 

subToBinOp Sub.BinaryAnd = Py.BinaryAnd noSpan 
subToBinOp Sub.BinaryOr = Py.BinaryOr noSpan 
subToBinOp Sub.Xor = Py.Xor noSpan 
subToBinOp Sub.ShiftLeft = Py.ShiftLeft noSpan 
subToBinOp Sub.ShiftRight = Py.ShiftRight noSpan


subToUnOp:: Sub.UnOp ->  Py.Op SrcSpan
subToUnOp Sub.Not  = Py.Not noSpan
subToUnOp Sub.Invert = Py.Invert noSpan