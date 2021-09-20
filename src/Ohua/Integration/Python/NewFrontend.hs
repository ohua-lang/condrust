{-# LANGUAGE InstanceSigs#-}
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
import Ohua.Integration.Python.Frontend.Convert (exprToSub)

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


viaSubToIR::(ConvertM m, MonadError Error m) => Py.Expr SrcSpan -> m (FrLang.Expr ty)
viaSubToIR  pyExpr =  do
    subExpr <- exprToSub pyExpr
    return $ subToIR subExpr

subToIR ::ConvertM m => Sub.Expr -> m (FrLang.Expr ty)
subToIR (Sub.Var bnd) = return $ VarE bnd
subToIR (Sub.Int int) = return $ LitE $ NumericLit int
subToIR (Sub.Bool bool) = return $ LitE $ BoolLit bool
subToIR Sub.None = return $  LitE  UnitLit
subToIR (Sub.Call (Sub.Pure bnd) args) = 
    let args' = map subArgToIR args
    in return $ AppE (VarE bnd) args'
subToIR (Sub.Call (Sub.Dotted objBnd funBnd) args) = 
    let args' = map subArgToIR args
        receiver = VarE objBnd
        receiverTy = Type PythonObject
        --argTypes = 
        method = LitE (FunRefLit (FunRef funBnd Nothing $ STFunType receiverTy (Left Unit)))
    in return $ BindE receiver method `AppE` args'

          
subArgToIR :: Sub.Argument  -> FrLang.Expr ty
subArgToIR (Sub.Arg expr) = subToIR expr

-- pretendTypesearch:: [a] -> Int

