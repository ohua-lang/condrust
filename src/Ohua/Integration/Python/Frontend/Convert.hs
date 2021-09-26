{-|
Module      : Python.Frontend.Convert
Description : Convert AST to supported python subset

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
suiteToSub stmts = Sub.PySuite  <$> mapM stmtToSub stmts

stmtToSub::(Monad m, MonadError Error m) => Py.Statement SrcSpan -> m Sub.Stmt
stmtToSub stmt@Py.Import{import_items=items} = unsupError "local imports" stmt
stmtToSub stmt@Py.FromImport{from_module= mod, from_items=items} =  unsupError "local imports" stmt
stmtToSub assign@(Py.Assign targets expr annot) = do
    targets' <- mapM exprToTarget targets
    expr' <- exprToSubExpr expr
    return $ Sub.Assign targets' expr'     
stmtToSub whileE@(Py.While cond do_block [] annE) = do
    cond' <- exprToSubExpr cond
    block' <- suiteToSub do_block
    return $ Sub.WhileStmt cond' block'
stmtToSub whileE@(Py.While cond do_block elseBlock annE) = unsupError "else blocks in while expressions" whileE
stmtToSub forE@(Py.For targets generator body [] annot) = do
    targets' <- mapM exprToTarget targets
    generator' <- exprToSubExpr generator
    body' <- suiteToSub body
    return $ Sub.ForStmt targets' generator' body'
stmtToSub forE@(Py.For _ _ _ elseBlock _) = unsupError "else blocks in for expressions" forE
{-Note: there's 2 complications with if's in python 
    1st: there's elIfs -> this probably not hard, it just means i have to nes translation
    2nd: there'r blocks inside and again, I can not exclude 'return' statements in python 
    -> I assume rust function execution continues, when an if-block return a value, this is not the case in python 

-}
stmtToSub ifElifElse@(Py.Conditional ifsAndSuites elseSuite annot ) = do
    ifs <- mapM (exprToSubExpr . fst) ifsAndSuites
    suites <-  mapM (suiteToSub . snd) ifsAndSuites
    elseSuite' <- suiteToSub elseSuite
    return $ Sub.CondStmt (zip ifs suites)  elseSuite'
stmtToSub stmt@(Py.StmtExpr expr annot) = do 
    expr' <- exprToSubExpr expr
    return $ Sub.StmtExpr expr'
stmtToSub ret@(Py.Return Nothing annot) = return $ Sub.Return Nothing
stmtToSub ret@(Py.Return (Just expr) annot) = do
    expr' <- exprToSubExpr expr
    return $ Sub.Return (Just expr')
-- Todo: is it valid to translate 'pass' to 'UnitLit'?
{- 'pass' as a function body -> equiv. to 'return None' -> works
    'pass' TL -> not relevant, we only look inside algos
    'pass' in a class defintion -> we don't touch those either, so that should be ok
    'pass' in a branch -> in the backend, the only point where 'UnitLit' is translated to
        'return None' or just 'return' is the end of a function block and even there we 
        could just write 'None'
- > Seems legit
-}
stmtToSub Py.Pass{} = return Sub.Pass

stmtToSub asyncFor@(Py.AsyncFor stmt annot) = unsupError "async for loops" asyncFor
stmtToSub funDef@Py.Fun{} = unsupError "inner function defintions" funDef
stmtToSub asyncFun@Py.AsyncFun{} = unsupError "async function definitions" asyncFun
stmtToSub classDef@Py.Class{} = unsupError "class defintions inside functions" classDef
stmtToSub augmAs@(Py.AugmentedAssign target operation expr annot) = unsupError "augmented assignments" augmAs
stmtToSub annotAs@(Py.AnnotatedAssign targetAnnot target expr stmtAnnot) = unsupError "annotated assignments" annotAs
stmtToSub dec@(Py.Decorated decorators funOrClass annot) = unsupError "decorators" dec


stmtToSub try@(Py.Try block excepts elseBlock finallyBlock annot)= unsupError "try except statement" try
stmtToSub raise@(Py.Raise raiseExor annot) = unsupError "exception raising statements" raise
stmtToSub with@(Py.With contextTuples block annot) = unsupError "with statements" with
stmtToSub asyncWith@(Py.AsyncWith stmt annot) = unsupError "async with statements" asyncWith
stmtToSub stmt@Py.Break{} = unsupError "break statements" stmt
stmtToSub stmt@Py.Continue{} = unsupError "continue statements" stmt
stmtToSub stmt@Py.Delete{} = unsupError "delete statements" stmt

-- TODO: We will probably never support this
stmtToSub stmt@Py.Global{} = unsupError "global keyword"stmt
stmtToSub stmt@Py.NonLocal{} = unsupError "nonlocal keyword"stmt
stmtToSub stmt@Py.Assert{} = unsupError "assertions"stmt
stmtToSub stmt@Py.Print{} = py2Error stmt
stmtToSub stmt@Py.Exec{} = unsupError "exec statements"stmt
{-

-}
exprToSubExpr :: (Monad m, MonadError Error m) => Py.Expr SrcSpan -> m Sub.Expr
exprToSubExpr Py.Var {var_ident=ident} =   return $ Sub.Var $ toBinding ident
exprToSubExpr (Py.Int val strRepr annot) = return $ Sub.Int val
exprToSubExpr (Py.Bool bool annot) =       return $ Sub.Bool bool
exprToSubExpr (Py.None annot) =            return Sub.None

exprToSubExpr (Py.Call fun args annot) = do
    call_fun <- exprToFRef fun
    args' <- mapM argToSub args
    return $ Sub.Call call_fun args'

exprToSubExpr (Py.BinaryOp op expr1 expr2 annot) = do
    op' <- binOpToSub op
    expr1' <- exprToSubExpr expr1
    expr2' <- exprToSubExpr expr2
    return $ Sub.BinaryOp op' expr1' expr2'

exprToSubExpr (Py.UnaryOp op expr annot) = do
    op' <- unOpToSub op
    expr1' <- exprToSubExpr expr
    return $ Sub.UnaryOp op' expr1'

exprToSubExpr (Py.CondExpr branch ifExpr elseBranch annot) = do
    condE <- exprToSubExpr ifExpr
    trueBranch <- exprToSubExpr branch
    falseBranch <- exprToSubExpr elseBranch
    return $ Sub.CondExpr condE trueBranch falseBranch

exprToSubExpr (Py.Tuple exprs annot) = do
    exprs' <- mapM exprToSubExpr exprs
    return $ Sub.Tuple exprs'

exprToSubExpr lL@Py.LongInt{} = unsupError "LongInts" lL
exprToSubExpr fL@(Py.Float valDbl strRepr annot) = unsupError "Floats" fL
exprToSubExpr imL@(Py.Imaginary valDbl strRepr annot) = unsupError "Imaginaries" imL
exprToSubExpr ellL@(Py.Ellipsis annot) = unsupError "Ellipsis Literals" ellL
exprToSubExpr bsL@(Py.ByteStrings bStrings annot) = unsupError "ByteString Literals" bsL
exprToSubExpr strsL@(Py.Strings strings annot) = unsupError "Strings Literals" strsL
exprToSubExpr ustrL@(Py.UnicodeStrings strings annot) = unsupError "UnicodeStrings Literals" ustrL
exprToSubExpr subSc@(Py.Subscript subscripted subscript annot) = unsupError "Subscript Expressions" subSc
exprToSubExpr slice@(Py.SlicedExpr sliced slices annot) = unsupError "Slicing Expressions" slice
-- TODO: Dotted's can be function references (in statefull calls) or attribute references
-- as the latter one is not supported we do not gerenally support Dot expressions
exprToSubExpr dot@(Py.Dot object attribute annot) = unsupError "Attribute references" dot
exprToSubExpr lam@(Py.Lambda params expr annot) = unsupError "Lambda Expressions" lam
exprToSubExpr yield@(Py.Yield mayBeArg annot) = unsupError "Yield Expressions" yield
exprToSubExpr gen@(Py.Generator comprehension annot) = unsupError "generator expression " gen
exprToSubExpr await@(Py.Await expr annot) = unsupError "await expression" await

-- TODO/Note: It would at first glance be possible to translate lists, sets and dictioniries to tuples
-- In a way, this enforces 'functional' usage of them i.e. recreating insted of mutating
--But: Problems in Frontend/for Transormations
    -- Slicing with nth would not work for dicts (I could build a workarround maybe for slice expressions in the frontend)
    -- Appending works differently for sets (no duplicates)
-- Problems in Backend: 
    -- I can not distiguish Tuples from 'Tupled-Containers' and calls
    --  like l.pop(), l.items(), l.values(), l.intersect() will fail  
exprToSubExpr list@(Py.List items annot) = unsupError "list expression" list
exprToSubExpr listComp@(Py.ListComp comprehension annot) = unsupError "list comprehensions" listComp
-- TODO: Could be converted to a list of tuples
-- ...but how could we distiguish real lists of tuples from dicts in the backend :-(
exprToSubExpr dict@(Py.Dictionary keysAndValues annot) = unsupError "dicts" dict
exprToSubExpr dictComp@(Py.DictComp compehension annot) = unsupError "dict comprehensions" dictComp
-- TODO: Could be a list, but we'd loose distinction in the backend as with dicts :-(
exprToSubExpr set@(Py.Set items annot) = unsupError "set expressions" set
exprToSubExpr setComp@(Py.SetComp comprehension annot) =  unsupError "set comprehensions" setComp
-- I think supporting this could get complicated if we want to have any controle over types
exprToSubExpr starred@(Py.Starred expr annot) = unsupError "Starred Expressions" starred
-- TODO/Question: I need to know all possible occurences of parenthesized expressionss and 
-- in how far there can be precedence or other semantic issues arrising from just using the inner expression
exprToSubExpr paren@(Py.Paren expr annot) = exprToSubExpr expr
exprToSubExpr strConv@(Py.StringConversion expr annot) = py2Error strConv


exprToFRef:: (Monad m, MonadError Error m) => Py.Expr SrcSpan -> m Sub.FRef
exprToFRef Py.Var{var_ident=ident} = return $ Sub.Pure (toBinding ident)
exprToFRef Py.Dot{dot_expr= dots,dot_attribute = ident}  = do
    let funBinding  = toQualBinding $ Py.ident_string ident
        objBinding = fromString $ chainBindings "" dots
    return $ Sub.Dotted objBinding funBinding
exprToFRef anyOther = unsupError "this kind of expression as function reference: " anyOther


{-
targetsToSub:: (Monad m, MonadError Error m) => [Py.Expr SrcSpan] -> m Sub.Target
targetsToSub [expr] = exprToTarget expr
targetsToSub (e:es) = Sub.Tpl <$> mapM (varOrFail <=< exprToTarget) (e:es)
-}


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
exprToTarget (Py.Tuple exprs annot) = Sub.Tpl <$> mapM (varOrFail <=< exprToTarget) exprs
exprToTarget (Py.Paren (Py.Tuple exprs an) ann) = Sub.Tpl <$> mapM (varOrFail <=< exprToTarget) exprs
{-Question: Can we have list patterns?-}
exprToTarget lst@(Py.List exprs annot) = unsupError "lists as patterns" lst
exprToTarget dot@(Py.Dot exprs termVar annot) = unsupError "attribute assignment" dot
-- Question: I assume it's troublesome for some reason to translate this (probably because in haskell 
-- we do not modify things but return new ones)...Why exactly?
exprToTarget subScr@(Py.Subscript subscriptee indexExpr annot) = unsupError "indexed patterns" subScr
exprToTarget slice@(Py.SlicedExpr subscriptee slices annot) = unsupError "slice patterns" slice
exprToTarget starred@(Py.Starred expr annot) = unsupError "starred expression patterns" starred
exprToTarget any = throwError $ "Encountered " <> show any <> " while trying to convert patterns. This is a bug"


varOrFail :: (Monad m, MonadError Error m) => Sub.Target -> m Binding
varOrFail pat =
         case pat of
            Sub.Single bnd -> return bnd
            _ -> unsupError "nested targets in for loops or assignments" pat

argToSub :: (Monad m, MonadError Error m) => Py.Argument SrcSpan -> m Sub.Argument
argToSub (Py.ArgExpr expr annot) = do
    argExpr <- exprToSubExpr expr
    return $ Sub.Arg argExpr
argToSub arg = unsupError "args and kwars" arg

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

makeLoopRef :: String -> SrcSpan -> String
-- TODO: Case we like the idea, propagate ScrSpan => a throug all fknts and
-- produce name based on coords here. 
-- Alternative make case distinction here if we may want to change the annotations
makeLoopRef loopKind loc = loopKind ++ "_"