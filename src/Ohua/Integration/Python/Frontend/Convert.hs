{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Python.Frontend.Convert
Description : Convert AST to supported subsetf

This module defines the mapping from the python AST to the 
subset of python the frontend currently supports
-}
module Ohua.Integration.Python.Frontend.Convert where

import Ohua.Prelude

import Ohua.Integration.Python.Frontend.Subset as Sub
import Ohua.Integration.Python.Util


import qualified Language.Python.Common.AST as Py
import Language.Python.Common (SrcSpan (SpanEmpty))
import Language.Python.Common.SrcLocation (SrcSpan)
import Language.Python.Common.AST (Expr(var_ident))



exprToSub :: (Monad m, MonadError Error m) => Py.Expr SrcSpan -> m Sub.Expr
exprToSub Py.Var {var_ident=ident} =   return $ Sub.Var $ toBinding ident
exprToSub (Py.Int val strRepr annot) = return $ Sub.Int val
exprToSub (Py.Bool bool annot) =       return $ Sub.Bool bool
exprToSub (Py.None annot) =            return Sub.None

exprToSub (Py.Call fun args annot) = do
    call_fun <- exprToFRef fun
    args' <- mapM argToSub args
    return $ Sub.Call call_fun args'



argToSub :: (Monad m, MonadError Error m) => Py.Argument SrcSpan -> m Sub.Argument
argToSub (Py.ArgExpr expr annot) = do
    argExpr <- exprToSub expr
    return $ Sub.Arg argExpr

exprToFRef:: (Monad m, MonadError Error m) => Py.Expr SrcSpan -> m Sub.FRef
exprToFRef Py.Var{var_ident=ident} = return $ Pure (toBinding ident)
exprToFRef Py.Dot{dot_expr= dots,dot_attribute = ident}  = do 
    let funBinding  = toQualBinding $ Py.ident_string ident
        objBinding = fromString $ chainBindings "" dots
    return $ Dotted objBinding funBinding

chainBindings:: String -> Py.Expr SrcSpan -> String
chainBindings lst (Py.Var ident anno) = Py.ident_string ident
chainBindings lst (Py.Dot expr ident annot) = 
    chainBindings (Py.ident_string ident++"."++ lst) expr