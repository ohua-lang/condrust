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
subToSuite = map subToStmt

subToStmt:: Sub.Stmt -> Py.Statement SrcSpan
subToStmt (Sub.WhileStmt expr suite) =
    Py.While (subToExpr expr) (subToSuite suite) [] noSpan
subToStmt (Sub.ForStmt targets generator suite) =
    Py.For
        (map subToExpr targets)
        (subToExpr generator)
        (subToSuite suite)
        []
        noSpan
subToStmt (Sub.CondStmt (cond, ifSuite) elseSuite) =
    Py.Conditional
        [(subToExpr cond, subToSuite ifSuite)]
        (subToSuite elseSuite)
        noSpan
subToStmt (Sub.StmtExpr expr) = Py.StmtExpr (subToExpr expr) noSpan
subToStmt (Sub.Assign exprs expr) =
    Py.Assign (map subToExpr exprs) (subToExpr expr) noSpan
subToStmt (Sub.AugmentedAssign assignee assignOp expr) =
    Py.AugmentedAssign
        (subToExpr assignee)
        (subToAssignOp assignOp)
        (subToExpr expr)
        noSpan



subToExpr:: Sub.Expr -> Py.Expr SrcSpan
subToExpr (Sub.Int i) = Py.Int{int_value=i, expr_literal=show i, expr_annot= noSpan}
subToExpr (Sub.Bool b) = Py.Bool{bool_value= b, expr_annot = noSpan}
subToExpr Sub.None = Py.None noSpan
subToExpr (Sub.Var bnd) = toPyVar . fromBinding $ bnd
subToExpr (Sub.Strings strings) = Py.Strings strings noSpan
subToExpr (Sub.Call expr args) =
    Py.Call
        (subToExpr expr)
        (map  subToArg args)
        noSpan
subToExpr (Sub.DotExpr expr qBnd) = Py.Dot (subToExpr expr) (convertQBind qBnd) noSpan
subToExpr (Sub.CondExpr condE thenE elseE) =
    Py.CondExpr
        (subToExpr condE)
        (subToExpr thenE)
        (subToExpr elseE)
        noSpan

subToExpr (Sub.Tuple exprs) = Py.Tuple (map subToExpr exprs) noSpan

subToExpr (Sub.TplSubscript bnd pos) =
    Py.Subscript
        (toPyVar. fromBinding $ bnd)
        (Py.Int pos (show pos) noSpan)
        noSpan
subToExpr (Sub.BinaryOp op expr1 expr2) =
    Py.BinaryOp (subToBinOp op) (subToExpr expr1) (subToExpr expr2) noSpan

subToExpr (Sub.UnaryOp op expr1) =
    Py.UnaryOp (subToUnOp op) (subToExpr expr1) noSpan



subToArg :: Sub.Argument -> Py.Argument SrcSpan
subToArg (Sub.Arg subExpr) = Py.ArgExpr (subToExpr subExpr) noSpan



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

subToAssignOp :: Sub.AssignOp -> Py.AssignOp SrcSpan
subToAssignOp Sub.PlusAssign  = Py.PlusAssign noSpan
subToAssignOp Sub.MinusAssign  = Py.MinusAssign noSpan

convertQBind :: QualifiedBinding -> Py.Ident SrcSpan
convertQBind (QualifiedBinding [] bnd) = fromBinding bnd