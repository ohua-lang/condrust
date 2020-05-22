module Ohua.Frontend.Convert.Rust where

import Ohua.Prelude

import Ohua.Frontend.Convert
import Ohua.Frontend.Lang as FrLang
import Language.Rust.Syntax as Rust
import Language.Rust.Data.Ident


instance ConvertFrom (Rust.Expr a) where 
    convertFrom Box{} = throwError "Currently, we do not support the construction of boxed values. Please do so in a function."
    convertFrom InPlace{} = throwError "Currently, we do not support in-place expressions."
    convertFrom Vec{} = throwError "Currently, we do not support array expressions. Please do so in a function."
    convertFrom (Call [] fun args _) = do
        fun' <- convertFrom fun
        args' <- mapM convertFrom args
        return $ fun' `AppE` args'
    convertFrom Call{} = throwError "Currently, we do not support attributes on function calls."
    convertFrom (MethodCall [] receiver method Nothing args _) = do
        receiver' <- convertFrom receiver
        let method' = convertIdent method
        args' <- mapM convertFrom args
        return (BindE receiver' method') `AppE` args
    convertFrom (MethodCall [] _ _ (Just _) _ _) = throwError "Currently, we do not support type parameters for function calls. Your best shot: wrap the call into a function."
    convertFrom MethodCall{} = throwError "Currently, we do not support attributes on method calls."
    convertFrom (TupExp [] vars _) = do
        vars' <- mapM converFrom vars
        return $ TupE vars'
    convertFrom TupExpr{} = throwError "Currently, we do not support attributes on tuple exressions."
    convertFrom (Binary [] op left right _) = do
        let op' = convertBinOp op
        left' <- convertFrom left
        right' <- convertFrom right
        return $ op' `AppE` [left', right']
    convertFrom Binary{} = throwError "Currently, we do not support attributes on binary operations."
    convertFrom (Unary [] op arg _) = do
        let op' = convertUnOp op
        arg' <- convertFrom arg
        return $ op' `AppE` [arg']
    convertFrom Unary{} = throwError "Currently, we do not support attributes on unary operations."
    -- TODO to be completed!

convertIdent :: Ident -> FrLang.Expr
convertIdent Ident{name=n, raw=False} = Lit $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) $ pack n) Nothing
-- TODO it seems we need a context here to understand the QualifiedPath here.
--      the question is whether we really need this???
convertIdent Ident{name=n, raw=True} = undefined 

convertBinOp :: BinOp -> FrLang.Expr
convertBinOp AddOp = toExpr "+"
convertBinOp SubOp = toExpr "-"
convertBinOp MulOp = toExpr "*"
convertBinOp DivOp = toExpr "/"
convertBinOp RemOp = toExpr "%"
convertBinOp AndOp = toExpr "&&"
convertBinOp OrOp  = toExpr "||"
convertBinOp BitXorOp  = toExpr "^"
convertBinOp BitAndOp  = toExpr "&"
convertBinOp BitOrOp  = toExpr "|"
convertBinOp ShlOp  = toExpr "<<"
convertBinOp ShrOp  = toExpr ">>"
convertBinOp EqOp  = toExpr "=="
convertBinOp LtOp  = toExpr "<"
convertBinOp LeOp  = toExpr "<="
convertBinOp NeOp  = toExpr "!="
convertBinOp GeOp  = toExpr ">="
convertBinOp GtOp  = toExpr ">"
    where 
        toExpr op = Lit $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) op) Nothing

convertUnOp :: UnOp -> FrLang.Expr
convertUnOp Deref = toExpr "*"
convertUnOp Not   = toExpr "!"
convertUnOp Neg   = toExpr "-"
    where 
        toExpr op = Lit $ FunRefLit $ FunRef (QualifiedBinding (makeThrow []) op) Nothing
