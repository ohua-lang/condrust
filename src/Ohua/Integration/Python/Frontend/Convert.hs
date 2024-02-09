{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-|
Module      : Python.Frontend.Convert
Description : Convert AST to supported python subset

This module defines the mapping from the python AST to the 
subset of python the frontend currently supports
-}
module Ohua.Integration.Python.Frontend.Convert where

import Ohua.Commons.Prelude

import Ohua.Integration.Python.Util
import qualified Ohua.Integration.Python.Frontend.Subset as Sub

import Language.Python.Common (SrcSpan)
import qualified Language.Python.Common.AST as Py


suiteToSub:: (Monad m, MonadError Error m) => Py.Suite SrcSpan -> m Sub.Suite
suiteToSub stmts = Sub.PySuite  <$> mapM stmtToSub stmts

suiteToBlock::(Monad m, MonadError Error m) => Py.Suite SrcSpan -> m Sub.Block
suiteToBlock = mapM (stmtToSub <=< isNoReturn)

stmtToSub::(Monad m, MonadError Error m) => Py.Statement SrcSpan -> m Sub.Stmt
stmtToSub stmt@Py.Import{} = unsupPyError "local imports" stmt
stmtToSub stmt@Py.FromImport{} =  unsupPyError "local imports" stmt
stmtToSub (Py.Assign [target] expr _annot) = do
    targets' <- exprToTarget target
    expr' <- exprToSubExpr expr
    return $ Sub.Assign targets' expr'
stmtToSub assign@(Py.Assign _targets _expr _annot) = unsupPyError "multiple variables in assignment. You may use a tuple instead" assign
stmtToSub (Py.While cond do_block [] _annot) = do
    cond' <- exprToSubExpr cond
    block' <- suiteToBlock do_block
    return $ Sub.WhileStmt cond' block'

stmtToSub whileE@(Py.While _cond _do_block _elseBlock _annot) = unsupPyError "else blocks in while expressions" whileE
stmtToSub (Py.For targets generator body [] _annot) = do
    targets' <- targetsToSub targets
    generator' <- exprToSubExpr generator
    body' <- suiteToBlock body
    return $ Sub.ForStmt targets' generator' body'
stmtToSub forE@(Py.For _ _ _ _elseBlock _) = unsupPyError "else blocks in for expressions" forE

{-Note: there's 2 complications with if-Statements in python 
    1st: there's elIfs -> this probably not hard, it just means i have to nest translation
    2nd: there'r blocks inside the branches. I can not exclude 'return' statements from blocks in general in python 
    -> I assume rust function execution continues, when an if-block return a value, this is not the case in python 
    -> I'll need a separate handling for non-function blocks
-}

stmtToSub ite@(Py.Conditional ifsAndSuites [] _annot ) = unsupPyError "if-statements without else blocks" ite

stmtToSub (Py.Conditional ifsAndSuites elseSuite _annot ) = do
    ifs <- mapM (exprToSubExpr . fst) ifsAndSuites
    blocks <-  mapM (suiteToBlock . snd) ifsAndSuites
    protoElse <- suiteToBlock elseSuite
    elseBlock <- do 
        case elseSuite of
            [] -> return Nothing 
            _block -> return $ Just protoElse
    return $ Sub.CondStmt (zip ifs blocks) elseBlock

stmtToSub (Py.StmtExpr expr _annot) = do
    expr' <- exprToSubExpr expr
    return $ Sub.StmtExpr expr'

stmtToSub (Py.Return Nothing _annot) = return $ Sub.Return Nothing
stmtToSub (Py.Return (Just expr) _annot) = do
    expr' <- exprToSubExpr expr
    return $ Sub.Return (Just expr')

{- Is it valid to translate 'pass' to 'UnitLit'?
    'pass' as a function body -> equiv. to 'return None' -> works
    'pass' TL -> not relevant, we only look inside algos
    'pass' in a class defintion -> we don't touch those either, so that should be ok
    'pass' in a branch -> in the backend, the only point where 'UnitLit' is translated to
        'return None' or just 'return' is the end of a function block and even there we 
        could just write 'None'
- > Seems legit
-}
stmtToSub Py.Pass{} = return Sub.Pass

stmtToSub asyncFor@(Py.AsyncFor _stmt _annot) = unsupPyError "async for loops" asyncFor
stmtToSub funDef@Py.Fun{} = unsupPyError "inner function defintions" funDef
stmtToSub asyncFun@Py.AsyncFun{} = unsupPyError "async function definitions" asyncFun
stmtToSub classDef@Py.Class{} = unsupPyError "class defintions inside functions" classDef
stmtToSub augmAs@(Py.AugmentedAssign _target _operation _expr _annot) = unsupPyError "augmented assignments" augmAs
stmtToSub annotAs@(Py.AnnotatedAssign _targetAnnot _target _expr _annot) = unsupPyError "annotated assignments" annotAs
stmtToSub dec@(Py.Decorated _decorators _funOrClass _annot) = unsupPyError "decorators" dec


stmtToSub pyTry@(Py.Try _block _excepts _elseBlock _finallyBlock _annot)= unsupPyError "try except statement" pyTry
stmtToSub raise@(Py.Raise _raiseExor _annot) = unsupPyError "exception raising statements" raise
stmtToSub with@(Py.With _contextTuples _block _annot) = unsupPyError "with statements" with
stmtToSub asyncWith@(Py.AsyncWith _stmt _annot) = unsupPyError "async with statements" asyncWith
stmtToSub stmt@Py.Break{} = unsupPyError "break statements" stmt
stmtToSub stmt@Py.Continue{} = unsupPyError "continue statements" stmt
stmtToSub stmt@Py.Delete{} = unsupPyError "delete statements" stmt

stmtToSub stmt@Py.Global{} = unsupPyError "global keyword"stmt
stmtToSub stmt@Py.NonLocal{} = unsupPyError "nonlocal keyword"stmt
stmtToSub stmt@Py.Assert{} = unsupPyError "assertions"stmt
stmtToSub stmt@Py.Print{} = py2Error stmt
stmtToSub stmt@Py.Exec{} = unsupPyError "exec statements"stmt


exprToSubExpr :: (Monad m, MonadError Error m) => Py.Expr SrcSpan -> m Sub.Expr
exprToSubExpr Py.Var {var_ident=ident} =   return $ Sub.Var $ toBinding ident
exprToSubExpr (Py.Int val _strRepr _annot) = return $ Sub.Int val
exprToSubExpr (Py.Bool bl _annot) =       return $ Sub.Bool bl
exprToSubExpr (Py.None _annot) =            return Sub.None

exprToSubExpr (Py.Call fun args _annot) = do
    call_fun <- exprToFRef fun
    args' <- mapM argToSub args
    return $ Sub.Call call_fun args'

exprToSubExpr (Py.BinaryOp op expr1 expr2 _annot) = do
    op' <- binOpToSub op
    expr1' <- exprToSubExpr expr1
    expr2' <- exprToSubExpr expr2
    return $ Sub.BinaryOp op' expr1' expr2'

exprToSubExpr (Py.UnaryOp op expr _annot) = do
    op' <- unOpToSub op
    expr1' <- exprToSubExpr expr
    return $ Sub.UnaryOp op' expr1'

exprToSubExpr (Py.CondExpr branch ifExpr elseBranch _annot) = do
    condE <- exprToSubExpr ifExpr
    trueBranch <- exprToSubExpr branch
    falseBranch <- exprToSubExpr elseBranch
    return $ Sub.CondExpr condE trueBranch falseBranch

exprToSubExpr (Py.Tuple exprs _annot) = do
    exprs' <- mapM exprToSubExpr exprs
    return $ Sub.Tuple exprs'

exprToSubExpr (Py.Lambda params expr _annot) = do
    params' <- mapM paramToSub params
    expr' <- exprToSubExpr expr
    return $ Sub.Lambda params' expr'

exprToSubExpr (Py.List exprs _annot) = do
    exprs' <- mapM exprToSubExpr exprs
    return $ Sub.List exprs'

exprToSubExpr (Py.Dictionary dictMappings _annot) = do
    exprs' <- mapM dictMapToSub dictMappings
    return $ Sub.Dict exprs'

exprToSubExpr (Py.Set exprs _annot) = do
    exprs' <- mapM exprToSubExpr exprs
    return $ Sub.Set exprs'

exprToSubExpr subSc@(Py.Subscript subscripted subscript _annot) = do 
    bnd <- exprToSubExpr subscripted
    key <- exprToSubExpr subscript
    case bnd of
        Sub.Var bnd' ->  return $ Sub.Subscript bnd' key
        _ -> unsupPyError "subscripting epressions on anything but a variable name" subSc


exprToSubExpr lL@Py.LongInt{} = return $ Sub.PyLiteral lL
exprToSubExpr fL@(Py.Float _valDbl _strRepr _annot) = return $ Sub.PyLiteral fL
exprToSubExpr imL@(Py.Imaginary _valDbl _strRepr _annot) = return $ Sub.PyLiteral imL
exprToSubExpr ellL@(Py.Ellipsis _annot) = return $ Sub.PyLiteral ellL
exprToSubExpr bsL@(Py.ByteStrings _bStrings _annot) = return $ Sub.PyLiteral bsL
exprToSubExpr strsL@(Py.Strings _strings _annot) = return $ Sub.PyLiteral strsL
exprToSubExpr ustrL@(Py.UnicodeStrings _strings _annot) = return $ Sub.PyLiteral ustrL

exprToSubExpr slice@(Py.SlicedExpr _sliced _slices _annot) = unsupPyError "Slicing Expressions" slice
exprToSubExpr dot@(Py.Dot _object _attribute _annot) = unsupPyError "Attribute references" dot
exprToSubExpr yield@(Py.Yield _mayBeArg _annot) = unsupPyError "Yield Expressions" yield
exprToSubExpr gen@(Py.Generator _comprehension _annot) = unsupPyError "generator expression " gen
exprToSubExpr await@(Py.Await _expr _annot) = unsupPyError "await expression" await

exprToSubExpr listComp@(Py.ListComp _comprehension _annot) = unsupPyError "list comprehensions" listComp
exprToSubExpr dictComp@(Py.DictComp _compehension _annot) = unsupPyError "dict comprehensions" dictComp
exprToSubExpr setComp@(Py.SetComp _comprehension _annot) =  unsupPyError "set comprehensions" setComp
exprToSubExpr starred@(Py.Starred _expr _annot) = unsupPyError "Starred Expressions" starred
-- TODO/Question: I need to know all possible occurences of parenthesized expressionss and 
-- in how far there can be precedence or other semantic issues arrising from just using the inner expression
exprToSubExpr (Py.Paren expr _annot) = exprToSubExpr expr
exprToSubExpr strConv@(Py.StringConversion _expr _annot) = py2Error strConv


exprToFRef:: (Monad m, MonadError Error m) => Py.Expr SrcSpan -> m Sub.FRef
exprToFRef Py.Var{var_ident=ident} = return $ Sub.Pure (toBinding ident)
exprToFRef Py.Dot{dot_expr= dots,dot_attribute = ident}  = do
    let funBinding  = toQualBinding .fromString $ Py.ident_string ident
        objBinding = fromString $ chainBindings "" dots
    return $ Sub.Dotted objBinding funBinding
exprToFRef (Py.Paren expr _annot) = exprToFRef expr
exprToFRef lE@Py.Lambda{} = do
    lE' <- exprToSubExpr lE
    return $ Sub.Direct lE'
exprToFRef anyOther = unsupPyError "this kind of expression as function reference: " anyOther



targetsToSub:: (Monad m, MonadError Error m) => [Py.Expr SrcSpan] -> m Sub.Target
targetsToSub [expr] = exprToTarget expr
targetsToSub (e:es) = Sub.Tpl <$> mapM (varOrFail <=< exprToTarget) (e:es)
targetsToSub _ = error "An empty target of assignment or for loop is no valid Python"



{- What can be a pattern in python? From the py grammar:
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
exprToTarget:: (Monad m, MonadError Error m) => Py.Expr SrcSpan -> m Sub.Target
exprToTarget Py.Var {var_ident=ident} = return $ Sub.Single $ toBinding ident
exprToTarget (Py.Tuple exprs _annot) = Sub.Tpl <$> mapM (varOrFail <=< exprToTarget) exprs
exprToTarget (Py.Paren (Py.Tuple exprs _an) _annot) = Sub.Tpl <$> mapM (varOrFail <=< exprToTarget) exprs
exprToTarget subScr@(Py.Subscript _subscriptee _indexExpr _annot) = Sub.Subscr <$> exprToSubExpr subScr

-- Question: Can we have list patterns?
exprToTarget lst@(Py.List _exprs _annot) = unsupPyError "lists as patterns" lst
exprToTarget dot@(Py.Dot _exprs _termVar _annot) = unsupPyError "attribute assignment" dot
-- Question: I assume it's troublesome for some reason to translate this (probably because in haskell 
-- we do not modify things but return new ones)...Why exactly?
exprToTarget slice@(Py.SlicedExpr _subscriptee _slices _annot) = unsupPyError "slice patterns" slice
exprToTarget starred@(Py.Starred _expr _annot) = unsupPyError "starred expression patterns" starred
exprToTarget anyE = throwError $ "Encountered " <> show anyE <> " while trying to convert patterns. This is a bug"

argToSub :: (Monad m, MonadError Error m) => Py.Argument SrcSpan -> m Sub.Argument
argToSub (Py.ArgExpr expr _annot) = do
    argExpr <- exprToSubExpr expr
    return $ Sub.Arg argExpr
argToSub arg = unsupPyError "args and kwars" arg

paramToSub ::  (Monad m, MonadError Error m) => Py.Parameter SrcSpan -> m Sub.Param
paramToSub (Py.Param ident _typeAnno _deflt _anno)  = return $ Sub.Param (toBinding ident)
-- paramToSub (Py.Param ident typeAnno Nothing anno)  = return $ Sub.Param (toBinding ident)
-- paramToSub dflt@(Py.Param ident typeAnno deflt anno) = unsupPyError "default values for paramters" dflt
paramToSub prm = unsupPyError "args, kwargs or keyword only parameters" prm

-- | Convert items of the iterable argument to dict creation. Those items can be (key, value) pairs or
-- | *dict i.e. copies of other dicts. 
dictMapToSub ::  (Monad m, MonadError Error m) => Py.DictKeyDatumList SrcSpan -> m (Sub.Expr, Sub.Expr)
dictMapToSub (Py.DictMappingPair key val) = do
    k' <- exprToSubExpr key
    v' <- exprToSubExpr val
    return (k', v')
dictMapToSub dU@(Py.DictUnpacking _dict) = unsupPyError "dict unpacking" dU


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
binOpToSub op  = error $ "Unexpected binary operation " <> show op


unOpToSub :: (Monad m, MonadError Error m) => Py.Op SrcSpan -> m Sub.UnOp
unOpToSub Py.Not{}  = return Sub.Not
unOpToSub Py.Invert{} = return Sub.Invert
unOpToSub op  = error $ "Unexpected unary operation " <> show op


isNoReturn:: (Monad m, MonadError Error m) => Py.Statement SrcSpan -> m (Py.Statement SrcSpan)
isNoReturn ret@Py.Return{} = unsupPyError "'return' anywhere but at the end of a function. Please use an assignment" ret
isNoReturn notAReturn = return notAReturn


varOrFail :: (Monad m, MonadError Error m) => Sub.Target -> m Binding
varOrFail pat =
         case pat of
            Sub.Single bnd -> return bnd
            _ -> throwError $ "Currently we do not support nested targets in for loops or assignments used in: "<> show pat


chainBindings:: String -> Py.Expr SrcSpan -> String
chainBindings _lst (Py.Var ident _anno) = Py.ident_string ident
chainBindings lst (Py.Dot expr ident _annot) =
    chainBindings (Py.ident_string ident++"."++ lst) expr
chainBindings _ _ = error "Currently we do not support function references other than variable names <x> or dotted expressions <x.y.z> "


makeLoopRef :: String -> SrcSpan -> String
makeLoopRef loopKind _loc = loopKind ++ "_"


