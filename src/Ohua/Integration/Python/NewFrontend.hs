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
import Ohua.Integration.Python.Frontend.Convert (stmtToSub, exprToSubExpr, binOpToSub, suiteToSub)

import qualified Language.Python.Common.AST as Py
import Language.Python.Common (SrcSpan (SpanEmpty))

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Control.Monad (Monad(return))



-- Keep track of names and types 
-- > will be useful when/if annotated types are passed through
type Context = HM.HashMap Binding Sub.PythonType
type ConvertM m = (Monad m, MonadState Context m)


-- viaSubToIR::(ConvertM m, MonadError Error m, CompM m) => Py.Expr SrcSpan -> m (FrLang.Expr ty)
viaSubToIR::(CompM m) => Py.Expr SrcSpan -> m (FrLang.Expr ty)
viaSubToIR  pyExpr =  do
    subExpr <- exprToSubExpr pyExpr
    subExprToIR subExpr

viaSubToIRStmt::(CompM m) => Py.Statement SrcSpan -> m (FrLang.Expr ty)
viaSubToIRStmt  pyStmt =  do
    subExpr <- stmtToSub pyStmt
    subStmtToIR subExpr

subSuiteToIR::(CompM m) => Sub.Suite -> m (FrLang.Expr ty)
subSuiteToIR any = undefined

subStmtToIR :: CompM m => Sub.Stmt -> m (FrLang.Expr ty)

subStmtToIR (Sub.WhileStmt expr suite) = do
    cond <- subExprToIR expr
    suite' <- subSuiteToIR suite
    let loopRef = "while_loop_body"
    let recursivePart= IfE cond (AppE (VarE loopRef) [])  (LitE UnitLit)
    return $ LetE (VarP loopRef) (LamE [] $ StmtE suite' recursivePart) recursivePart

subStmtToIR (Sub.ForStmt targets generator suite) = do
    targets' <- mapM subTargetsToIR targets 
    generator' <- subExprToIR generator 
    suite <- subSuiteToIR suite
    return $ MapE (LamE targets' suite) generator'

subStmtToIR (Sub.CondStmt [(cond, suite)] elseSuite) = do
    cond' <- subExprToIR cond
    suite' <- subSuiteToIR suite
    elseSuite' <- subSuiteToIR elseSuite
    return $ IfE cond' suite' elseSuite'

subStmtToIR (Sub.CondStmt ifsAndSuits elseSuite) = do
    let ((ifE, suite):elifs) = ifsAndSuits
    condE <- subExprToIR ifE
    trueBranch <-  subSuiteToIR suite
    falseBranch <-  subStmtToIR (Sub.CondStmt elifs elseSuite)
    return $ IfE condE trueBranch falseBranch

subStmtToIR (Sub.StmtExpr expr) = subExprToIR expr
subStmtToIR Sub.Pass = return $ LitE UnitLit
subStmtToIR any = convError any

-- subExprToIR ::ConvertM m => Sub.Expr -> m (FrLang.Expr ty)
subExprToIR :: CompM m => Sub.Expr -> m (FrLang.Expr ty)
subExprToIR (Sub.Var bnd) = return $ VarE bnd
subExprToIR (Sub.Int int) = return $ LitE $ NumericLit int
subExprToIR (Sub.Bool bool) = return $ LitE $ BoolLit bool
subExprToIR Sub.None = return $  LitE  UnitLit
subExprToIR (Sub.Call (Sub.Pure bnd) args) = do
    args'<- mapM subArgToIR args
    return $ AppE (VarE bnd) args'
{-
subExprToIR (Sub.Call (Sub.Dotted objBnd funBnd) args) = do
    args' <- mapM subArgToIR args
    let receiver = VarE objBnd
        receiverTy = Type PythonObject
        argTypes = 3
        method = LitE (FunRefLit (FunRef funBnd Nothing $ STFunType receiverTy (Left Unit)))
    return $ BindE receiver method `AppE` args'-}

subExprToIR (Sub.CondExpr condE trueExpr falseExpr) = do
    cond <- subExprToIR condE
    true <- subExprToIR trueExpr
    false <- subExprToIR falseExpr
    return $ IfE cond true false

subExprToIR (Sub.BinaryOp binOp expr1 expr2) = do
    op' <- subBinOpToIR binOp
    expr1' <- subExprToIR expr1
    expr2' <- subExprToIR expr2
    return $ op' `AppE` [expr1', expr2']

subExprToIR (Sub.UnaryOp unOp expr1) = do
    op' <- subUnOpToIR unOp
    expr1' <- subExprToIR expr1
    return $ op' `AppE` [expr1']

subExprToIR (Sub.Tuple exprs) = do
    exprs' <- mapM subExprToIR exprs
    return $ TupE exprs'

subExprToIR any =  convError any

-- subArgToIR :: ConvertM m => Sub.Argument -> m ( FrLang.Expr ty)
subArgToIR :: CompM m => Sub.Argument -> m ( FrLang.Expr ty)
subArgToIR (Sub.Arg expr) = subExprToIR expr

subTargetsToIR :: CompM m => Sub.Target -> m FrLang.Pat
subTargetsToIR (Sub.Single bnd) = return $ VarP bnd 
subTargetsToIR (Sub.Tpl bnds) = 
    let vars = map VarP bnds
    in return $ TupP vars

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

convError any = throwError $ "This shouldn't happen. Please file a bug about"
                <>" missing conversion for the subset expression: " <> show any