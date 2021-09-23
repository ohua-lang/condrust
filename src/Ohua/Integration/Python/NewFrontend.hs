{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|Slowly move frontend covnersion to this file withou messing up the 
old frontend to much
|-}
module Ohua.Integration.Python.NewFrontend where

import Ohua.Prelude
import Ohua.Frontend.Lang as FrLang

import qualified Ohua.Integration.Python.Frontend.Subset as Sub
import Ohua.Integration.Python.TypeExtraction
import Ohua.Integration.Python.Frontend.Convert (exprToSub, binOpToSub)

import qualified Language.Python.Common.AST as Py
import Language.Python.Common (SrcSpan (SpanEmpty))

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
-- import Ohua.Types.Error (Error)


-- Keep track of names and types 
-- > will be useful when/if annotated types are passed trough
type Context = HM.HashMap Binding Sub.PythonType
type ConvertM m = (Monad m, MonadState Context m)


-- viaSubToIR::(ConvertM m, MonadError Error m, CompM m) => Py.Expr SrcSpan -> m (FrLang.Expr ty)
viaSubToIR::(CompM m) => Py.Expr SrcSpan -> m (FrLang.Expr ty)
viaSubToIR  pyExpr =  do
    subExpr <- exprToSub pyExpr
    subToIR subExpr

-- subToIR ::ConvertM m => Sub.Expr -> m (FrLang.Expr ty)
subToIR ::CompM m => Sub.Expr -> m (FrLang.Expr ty)
subToIR (Sub.Var bnd) = return $ VarE bnd
subToIR (Sub.Int int) = return $ LitE $ NumericLit int
subToIR (Sub.Bool bool) = return $ LitE $ BoolLit bool
subToIR Sub.None = return $  LitE  UnitLit
subToIR (Sub.Call (Sub.Pure bnd) args) = do
    args'<- mapM subArgToIR args
    return $ AppE (VarE bnd) args'
{-
subToIR (Sub.Call (Sub.Dotted objBnd funBnd) args) = do
    args' <- mapM subArgToIR args
    let receiver = VarE objBnd
        receiverTy = Type PythonObject
        argTypes = 3
        method = LitE (FunRefLit (FunRef funBnd Nothing $ STFunType receiverTy (Left Unit)))
    return $ BindE receiver method `AppE` args'-}

subToIR (Sub.CondExpr condE trueExpr falseExpr) = do
    cond <- subToIR condE
    true <- subToIR trueExpr 
    false <- subToIR falseExpr
    return $ IfE cond true false

subToIR (Sub.BinaryOp binOp expr1 expr2) = do
    op' <- subBinOpToIR binOp
    expr1' <- subToIR expr1
    expr2' <- subToIR expr2
    return $ op' `AppE` [expr1', expr2']

subToIR (Sub.UnaryOp unOp expr1) = do
    op' <- subUnOpToIR unOp
    expr1' <- subToIR expr1
    return $ op' `AppE` [expr1']

subToIR (Sub.Tuple exprs) = do 
    exprs' <- mapM subToIR exprs 
    return $ TupE exprs'

          
-- subArgToIR :: ConvertM m => Sub.Argument -> m ( FrLang.Expr ty)
subArgToIR :: CompM m => Sub.Argument -> m ( FrLang.Expr ty)
subArgToIR (Sub.Arg expr) = subToIR expr

subBinOpToIR:: CompM m => Sub.BinOp -> m ( FrLang.Expr ty) 
subBinOpToIR Sub.Plus = toFunRefLit "+"
subBinOpToIR Sub.Minus = toFunRefLit "-"
subBinOpToIR Sub.Multiply = toFunRefLit "*"
subBinOpToIR Sub.Divide = toFunRefLit "/"
subBinOpToIR Sub.FloorDivide = toFunRefLit "//"
subBinOpToIR Sub.Modulo = toFunRefLit "%"
subBinOpToIR Sub.Exponent = toFunRefLit "**"
subBinOpToIR Sub.MatrixMult = toFunRefLit "@"

subBinOpToIR Sub.And = toFunRefLit "and"
subBinOpToIR Sub.Or  = toFunRefLit "or"
subBinOpToIR Sub.In = toFunRefLit "in"
subBinOpToIR Sub.Is = toFunRefLit "is"
subBinOpToIR Sub.IsNot = toFunRefLit "is not"
subBinOpToIR Sub.NotIn = toFunRefLit "not in"

subBinOpToIR Sub.LessThan = toFunRefLit "<"
subBinOpToIR Sub.GreaterThan = toFunRefLit ">"
subBinOpToIR Sub.Equality  = toFunRefLit "=="
subBinOpToIR Sub.GreaterThanEquals = toFunRefLit ">="
subBinOpToIR Sub.LessThanEquals = toFunRefLit "<="
subBinOpToIR Sub.NotEquals = toFunRefLit "!="

subBinOpToIR Sub.BinaryAnd = toFunRefLit "&"
subBinOpToIR Sub.BinaryOr = toFunRefLit "|"
subBinOpToIR Sub.Xor = toFunRefLit "^"
subBinOpToIR Sub.ShiftLeft = toFunRefLit "<<"
subBinOpToIR Sub.ShiftRight = toFunRefLit ">>"


subUnOpToIR:: CompM m => Sub.UnOp -> m ( FrLang.Expr ty) 
subUnOpToIR Sub.Not  = toFunRefLit "not"
subUnOpToIR Sub.Invert = toFunRefLit "~"

toFunRefLit :: Monad m => Binding -> m (FrLang.Expr ty)
{-- Note: Turns given string representation into literal expression representig an untyped, 
'unscoped' (the empty list in as Binding argument) function reference 
-- Question: why untyped ? --}
toFunRefLit string_repr = return $
                        LitE $ FunRefLit $
                        FunRef (QualifiedBinding (makeThrow []) string_repr) Nothing Untyped
