{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Python.Frontend.Convert
Description : Convert AST to supported subsetf

This module defines the mapping from the python AST to the 
subset of python the frontend currently supports
-}
module Ohua.Integration.Python.Frontend.Convert where

import Ohua.Prelude

import qualified Ohua.Integration.Python.Frontend.Subset as Sub
import Ohua.Integration.Python.Util


import qualified Language.Python.Common.AST as Py
import Language.Python.Common (SrcSpan (SpanEmpty))
import Language.Python.Common.SrcLocation (SrcSpan)
import Language.Python.Common.AST (Expr(var_ident))


suiteToSub:: (Monad m, MonadError Error m) => Py.Suite SrcSpan -> m Sub.Suite 
suiteToSub stmts = Sub.PySuite  <$> mapM stmtsToSub stmts

stmtsToSub::(Monad m, MonadError Error m) => Py.Statement SrcSpan -> m Sub.Stmt
stmtsToSub any = undefined

exprToSub :: (Monad m, MonadError Error m) => Py.Expr SrcSpan -> m Sub.Expr
exprToSub Py.Var {var_ident=ident} =   return $ Sub.Var $ toBinding ident
exprToSub (Py.Int val strRepr annot) = return $ Sub.Int val
exprToSub (Py.Bool bool annot) =       return $ Sub.Bool bool
exprToSub (Py.None annot) =            return Sub.None

exprToSub (Py.Call fun args annot) = do
    call_fun <- exprToFRef fun
    args' <- mapM argToSub args
    return $ Sub.Call call_fun args'

exprToSub (Py.BinaryOp op expr1 expr2 annot) = do
    op' <- binOpToSub op
    expr1' <- exprToSub expr1
    expr2' <- exprToSub expr2
    return $ Sub.BinaryOp op' expr1' expr2'

exprToSub (Py.UnaryOp op expr annot) = do
    op' <- unOpToSub op
    expr1' <- exprToSub expr
    return $ Sub.UnaryOp op' expr1' 

exprToSub (Py.CondExpr branch ifExpr elseBranch annot) = do
    condE <- exprToSub ifExpr
    trueBranch <- exprToSub branch
    falseBranch <- exprToSub elseBranch
    return $ Sub.CondExpr condE trueBranch falseBranch

exprToSub (Py.Tuple exprs annot) = do
    exprs' <- mapM exprToSub exprs 
    return $ Sub.Tuple exprs' 

exprToSub lL@Py.LongInt{} = unsupError "LongInts" lL
exprToSub fL@(Py.Float valDbl strRepr annot) = unsupError "Floats" fL
exprToSub imL@(Py.Imaginary valDbl strRepr annot) = unsupError "Imaginaries" imL
exprToSub ellL@(Py.Ellipsis annot) = unsupError "Ellipsis Literals" ellL
exprToSub bsL@(Py.ByteStrings bStrings annot) = unsupError "ByteString Literals" bsL
exprToSub strsL@(Py.Strings strings annot) = unsupError "Strings Literals" strsL
exprToSub ustrL@(Py.UnicodeStrings strings annot) = unsupError "UnicodeStrings Literals" ustrL
exprToSub subSc@(Py.Subscript subscripted subscript annot) = unsupError "Subscript Expressions" subSc
exprToSub slice@(Py.SlicedExpr sliced slices annot) = unsupError "Slicing Expressions" slice
-- TODO: Dotted's can be function references (in statefull calls) or attribute references
-- as the latter one is not supported we do not gerenally support Dot expressions
exprToSub dot@(Py.Dot object attribute annot) = unsupError "Attribute references" dot
exprToSub lam@(Py.Lambda params expr annot) = unsupError "Lambda Expressions" lam
exprToSub yield@(Py.Yield mayBeArg annot) = unsupError "Yield Expressions" yield
exprToSub gen@(Py.Generator comprehension annot) = unsupError "generator expression " gen
exprToSub await@(Py.Await expr annot) = unsupError "await expression" await

-- TODO/Note: It would at first glance be possible to translate lists, sets and dictioniries to tuples
-- In a way, this enforces 'functional' usage of them i.e. recreating insted of mutating
--But: Problems in Frontend/for Transormations
    -- Slicing with nth would not work for dicts (I could build a workarround maybe for slice expressions in the frontend)
    -- Appending works differently for sets (no duplicates)
-- Problems in Backend: 
    -- I can not distiguish Tuples from 'Tupled-Containers' and calls
    --  like l.pop(), l.items(), l.values(), l.intersect() will fail  
exprToSub list@(Py.List items annot) = unsupError "list expression" list
exprToSub listComp@(Py.ListComp comprehension annot) = unsupError "list comprehensions" listComp
-- TODO: Could be converted to a list of tuples
-- ...but how could we distiguish real lists of tuples from dicts in the backend :-(
exprToSub dict@(Py.Dictionary keysAndValues annot) = unsupError "dicts" dict
exprToSub dictComp@(Py.DictComp compehension annot) = unsupError "dict comprehensions" dictComp
-- TODO: Could be a list, but we'd loose distinction in the backend as with dicts :-(
exprToSub set@(Py.Set items annot) = unsupError "set expressions" set
exprToSub setComp@(Py.SetComp comprehension annot) =  unsupError "set comprehensions" setComp
-- I think supporting this could get complicated if we want to have any controle over types
exprToSub starred@(Py.Starred expr annot) = unsupError "Starred Expressions" starred
-- TODO/Question: I need to know all possible occurences of parenthesized expressionss and 
-- in how far there can be precedence or other semantic issues arrising from just using the inner expression
exprToSub paren@(Py.Paren expr annot) = exprToSub expr
exprToSub strConv@(Py.StringConversion expr annot) = py2Error strConv


exprToFRef:: (Monad m, MonadError Error m) => Py.Expr SrcSpan -> m Sub.FRef
exprToFRef Py.Var{var_ident=ident} = return $ Sub.Pure (toBinding ident)
exprToFRef Py.Dot{dot_expr= dots,dot_attribute = ident}  = do 
    let funBinding  = toQualBinding $ Py.ident_string ident
        objBinding = fromString $ chainBindings "" dots
    return $ Sub.Dotted objBinding funBinding

--Todo: What can be a pattern in python?
{- From the py grammar:
target          ::=  identifier
                     | "(" target_list ")"
                     | "[" target_list "]"
                     | attributeref
                     | subscription
                     | slicing
                     | "*" target
-}
{-Question: In contrast to Rust, there is no explicit 'wildcard' in the syntax i.e. '_' is just 
a normal Var. Should I separate this in th subset?-}
exprToPattern:: (Monad m, MonadError Error m) => Py.Expr SrcSpan -> m Sub.Pat 
exprToPattern Py.Var {var_ident=ident} = return $ Sub.VarP $ toBinding ident
exprToPattern (Py.Tuple exprs annot) =
    let varOrFail pat =
         case pat of
            Sub.VarP bnd -> return bnd
            _ -> unsupError "nested tuple patterns" pat
     in Sub.TupP <$> mapM (varOrFail <=< exprToPattern) exprs
{-Question: Can we have list patterns?-}
exprToPattern lst@(Py.List exprs annot) = unsupError "lists as patterns" lst
exprToPattern dot@(Py.Dot exprs termVar annot) = unsupError "attribute assignment" dot
-- Question: I assume it's troublesome for some reason to translate this (probably because in haskell 
-- we do not modify things but return new ones)...Why exactly?
exprToPattern subScr@(Py.Subscript subscriptee indexExpr annot) = unsupError "indexed patterns" subScr
exprToPattern slice@(Py.SlicedExpr subscriptee slices annot) = unsupError "slice patterns" slice
exprToPattern starred@(Py.Starred expr annot) = unsupError "starred expression patterns" starred
exprToPattern any = throwError $ "Encountered " <> show any <> " while trying to convert patterns. This is a bug"


argToSub :: (Monad m, MonadError Error m) => Py.Argument SrcSpan -> m Sub.Argument
argToSub (Py.ArgExpr expr annot) = do
    argExpr <- exprToSub expr
    return $ Sub.Arg argExpr

paramToSub ::  (Monad m, MonadError Error m) => Py.Parameter SrcSpan -> m Sub.Param
paramToSub (Py.Param ident typeAnno Nothing anno)  = return $ Sub.Param (toBinding ident)
paramToSub dflt@(Py.Param ident typeAnno deflt anno) = unsupError "default values for paramters" dflt
paramToSub prm = unsupError "args, kwargs or keyword only parameters" prm

binOpToSub :: (Monad m, MonadError Error m) => Py.Op SrcSpan -> m Sub.BinOp
binOpToSub Py.Plus{} = return Sub.Plus
binOpToSub Py.Minus{} = return Sub.Minus
binOpToSub Py.Multiply{} = return Sub.Multiply
binOpToSub Py.Divide{} = return Sub.Divide
binOpToSub Py.FloorDivide{} = return Sub.FloorDivide
binOpToSub Py.Modulo{} = return Sub.Modulo
binOpToSub Py.Exponent{} = return Sub.Exponent
binOpToSub Py.MatrixMult{} = return Sub.MatrixMult

binOpToSub Py.And{} = return Sub.And
binOpToSub Py.Or{}  = return Sub.Or
binOpToSub Py.In{} = return Sub.In
binOpToSub Py.Is{} = return Sub.Is
binOpToSub Py.IsNot{} = return Sub.IsNot
binOpToSub Py.NotIn{} = return Sub.NotIn

binOpToSub Py.LessThan{} = return Sub.LessThan
binOpToSub Py.GreaterThan{} = return Sub.GreaterThan
binOpToSub Py.Equality{}  = return Sub.Equality
binOpToSub Py.GreaterThanEquals{} = return Sub.GreaterThanEquals
binOpToSub Py.LessThanEquals{} = return Sub.LessThanEquals
binOpToSub Py.NotEquals{} = return Sub.NotEquals

binOpToSub Py.BinaryAnd{} = return Sub.BinaryAnd
binOpToSub Py.BinaryOr{} = return Sub.BinaryOr
binOpToSub Py.Xor{} = return Sub.Xor
binOpToSub Py.ShiftLeft{} = return Sub.ShiftLeft
binOpToSub Py.ShiftRight{} = return Sub.ShiftRight


unOpToSub :: (Monad m, MonadError Error m) => Py.Op SrcSpan -> m Sub.UnOp
unOpToSub Py.Not{}  = return Sub.Not
unOpToSub Py.Invert{} = return Sub.Invert


chainBindings:: String -> Py.Expr SrcSpan -> String
chainBindings lst (Py.Var ident anno) = Py.ident_string ident
chainBindings lst (Py.Dot expr ident annot) = 
    chainBindings (Py.ident_string ident++"."++ lst) expr


unsupError :: (MonadError e m, Semigroup e, IsString e, Show a1) => e -> a1 -> m a2
unsupError text expr = throwError $ "Currently we do not support "<> text <>" used in: "<> show expr

--TODO: can this be my responsibility in any way or redirect to bjpop@csse.unimelb.edu.au here ?
py2Error expr = throwError $ "For whatever reason you managed to get the exclusively version 2 expression "
                                <> show expr <> " through the python3 parser of language-python."
