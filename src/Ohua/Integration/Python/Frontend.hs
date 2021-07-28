{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Question: What are these extensions for? 
-- InstanceSigs removes error on type decls in implementation
-- Scoped..'binds' all type variables with same name inside a scope (eg functions in functions)

module Ohua.Integration.Python.Frontend where

import Ohua.Prelude

import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Types
import Ohua.Frontend.Convert
import Ohua.Frontend.PPrint ()

import Ohua.Integration.Lang
import Ohua.Integration.Python.Types
import Ohua.Integration.Python.Util
import Ohua.Integration.Python.TypeExtraction

import qualified Language.Python.Common.AST as Py
import Language.Python.Common (SrcSpan (SpanEmpty), startCol)

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE


type PythonNamespace = Namespace (FrLang.Expr (PythonArgType SrcSpan)) (Py.Statement SrcSpan)

instance Integration (Language 'Python) where
    type NS (Language 'Python) = Module
    type Type (Language 'Python) =  PythonArgType SrcSpan
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan


-- Note: Produces namespace later refered to with/required as 'ohuaNS^.algos' and 'ohuaNS^.imports'
-- TOdo: 1. PythonSubset als data anlegen
-- Todo : 2. Python AST auf Subset Mappen 
-- Todo : 3 Subset auf IR mappen 
-- => loadNS sollte 3°2 = 3(2()) => 3 . 2 sein
    loadNs :: CompM m => Language 'Python -> FilePath -> m (Module, PythonNamespace)
    loadNs _ srcFile = do
            mod <- liftIO $ load srcFile
            ns <- extractNs mod
            return (Module srcFile mod, ns)
            where
                extractNs :: CompM m => Py.Module SrcSpan -> m PythonNamespace
                extractNs (Py.Module statements) = do
                    imports <- concat . catMaybes <$>
                            mapM
                                (\case
                                    imp@Py.Import{} -> Just . toList <$> extractImports [] (Py.import_items imp)
                                    -- frImp@Py.FromImport -> Just . toList <$> extractRelativeImports frImp
                                    _ -> return Nothing)
                                statements
                    algos <- catMaybes <$>
                            mapM
                                (\case
                                    fun@Py.Fun{} ->
                                        Just . (\e -> Algo (toBinding$ Py.fun_name fun) e fun) <$> extractAlgo fun
                                    --TODO:functions inside classes
                                    --classFun@Py.Class{}
                                    -- Classes just contain Suites inside which memeber functions are just Py.Fun{} i.e.
                                    -- 'self' must be extracted from the arguments by ident_string
                                    _ -> return Nothing)
                                statements
                    return $ Namespace (filePathToNsRef srcFile) imports algos

                extractAlgo :: CompM m => Py.Statement SrcSpan -> m (FrLang.Expr (PythonArgType SrcSpan))
                extractAlgo function = do
                    args' <- mapM convertPat (Py.fun_args function)
                    block' <- convertExpr (Py.fun_body function)
                    return $ LamE args' block'

                extractImports::CompM m => [Binding] -> [Py.ImportItem a] -> m (NonEmpty Import)
                extractImports = undefined

                extractRelativeImports::CompM m => [Binding] -> [Py.ImportItem a] -> m (NonEmpty Import)
                extractRelativeImports = undefined


    loadTypes :: CompM m => Language 'Python ->
                    Module ->
                    PythonNamespace ->
                    m PythonNamespace
    -- TODO: Can meanswhile be replaced by id function
    loadTypes lang (Module filepath pymodule) ohuaNS = do
        -- Alles was vor update epressions steht ist dafür da, funnctionstypen aus deklarartionen (aus versch. Dateien im comilation scope zu popeln)
        -- _> ich hole mir die typen aus den call und kann mir daher den ersten Teil erstmal sparen
        filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNS^.algos)
        let filesAndPaths' = map (first convertOwn) filesAndPaths
        fun_types <- typesFromNS $ concatMap fst filesAndPaths'
        types' <- HM.fromList <$> mapM (verifyAndRegister fun_types) filesAndPaths'
        updateExprs ohuaNS (transformM (assignTypes types'))
        where
            funsForAlgo :: CompM m => Algo (FrLang.Expr (PythonArgType SrcSpan)) (Py.Statement SrcSpan)
                    -> m [([NSRef], QualifiedBinding)]
            -- extracts function literals from code and extracts for each the function
            -- type ()
            funsForAlgo (Algo _name code annotation) = do
                return []


            convertOwn :: [NSRef] -> [NSRef]
            convertOwn [] = [filePathToNsRef filepath]
            convertOwn n = n

            typesFromNS :: CompM m => [NSRef] -> m FunTypes
            typesFromNS nsRefs = HM.unions <$> mapM (extractFromFile . toFilePath . (,".py") ) nsRefs

            verifyAndRegister :: CompM m => FunTypes -> ([NSRef], QualifiedBinding)
                        -> m (QualifiedBinding, FunType (PythonArgType SrcSpan))
            verifyAndRegister fun_types ([candidate], qB@(QualifiedBinding _ qBName)) = undefined 

            assignTypes :: CompM m => FunTypes -> FrLang.Expr (PythonArgType SrcSpan) -> m (FrLang.Expr (PythonArgType SrcSpan))
            -- Todo: I don't get types right here :-/. How to get ArgType(PythonArgType SrcSpan)
            --  instead of  Expr (PythonArgType SrcSpan)
            assignTypes funTypes = \case
                (AppE (LitE (FunRefLit (FunRef qBinding funID _))) args) -> 
                    case args of
                        --at this point there should be no empty args, because empty calls are filled with call(Unit)
                        [] -> throwError "Compiler invariant broken."
                        [LitE UnitLit] -> return $ AppE (LitE $ FunRefLit $ FunRef qBinding funID $ FunType $ Left Unit) args
                        (a:args') -> 
                            return $ 
                            AppE (
                                LitE $ FunRefLit $ FunRef qBinding funID $ FunType $ 
                                Right $ map (Type $ PythonObject ) a:|args') args 
                e ->  return e

            globs :: [NSRef]
            -- Question: Glob is an Import that represents 'path that imports all bindings in this path.'
            -- Can I get an example?
            globs = mapMaybe (\case (Glob n) -> Just n; _ -> Nothing) (ohuaNS^.imports)


    {-instance ConvertPat ...Turns out, pattern matching is under way \o/ (PEP 634)-}

instance (Show a) => ConvertPat (Py.Parameter a) where
    -- Question: Argument conversion was commented with FIXME to attach (type) info 
    -- > why can't we make VarP have an additional Maybe ?  
    convertPat params@Py.Param{param_name=ident} = return $ VarP $ toBinding ident
    -- Question: Variables can be anything anyway. 
    -- We'll have to tread them like objects as they'r only 'frozen at the surface'. 
    -- So just prepend '*'/'**' to their names (to transfer unpacking to backend)? 
    convertPat params@Py.VarArgsPos{param_name=ident} = return $ VarP $ fromString $ "*"++Py.ident_string ident
    convertPat params@Py.VarArgsKeyword{param_name=ident} = return $ VarP $ fromString $ "**"++Py.ident_string ident
    -- Question: I don't think UnitP is a good idea here. Actually it should just map to nothing and converting on the backend should just include addding
    -- EndPositional again -> How to map to nothing wihtout failing?
    convertPat params@Py.EndPositional{} = throwError "hit EndPositional parameter. I thought they where a myth"

--Question: What else can be pattern in python?
instance (Show a) => ConvertPat (Py.Expr a) where
    -- Question: why are wilcards not different from normal vars i.e. also a VarP ...I could deref them anywhere ?
    convertPat Py.Var {var_ident=ident, expr_annot=_expr_annot} = return $ VarP $ toBinding ident
    -- Question: targets of a loop come as lists of expressions. Rust's convertPat says 'currently no list patterns'
    -- I could check if there's only one expr in the loop target, but what's wrong with making it a tuple pattern? 
    convertPat lst@(Py.List [expr] annot) = convertPat expr
    convertPat lst@(Py.List exprs annot) = do
        patterns <- mapM convertPat exprs
        return $ TupP patterns
    convertPat lst@(Py.Tuple exprs annot) = do
        patterns <- mapM convertPat exprs
        return $ TupP patterns

instance (Show a) => ConvertExpr (Py.Expr a) where
    convertExpr Py.Var{var_ident= ident} = return $ VarE $ toBinding ident
    convertExpr (Py.UnaryOp operation arg annot) = do
        op' <- convertExpr operation
        arg' <- convertExpr arg
        return $ op' `AppE` [arg']
    convertExpr (Py.BinaryOp operator left right annot) = do
        op' <- convertExpr operator
        left' <- convertExpr left
        right' <- convertExpr right
        return $ op' `AppE` [left', right']
    convertExpr (Py.Call fun args annot)= do
        fun' <- convertExpr fun
        args' <- mapM convertExpr args
        return $ fun' `AppE` args'


instance (Show a) => ConvertExpr (Py.Argument a) where
    convertExpr Py.ArgExpr{arg_expr=expr} = convertExpr expr
    convertExpr a@Py.ArgVarArgsPos{arg_expr=expr} = unsupError "*args in class construction" a
    convertExpr a@Py.ArgVarArgsKeyword {arg_expr=_arg_expr} = unsupError "**kwargs in class construction" a


instance (Show a) => ConvertExpr (Py.Statement a) where
-- Question: What's the matter with ItemStmt's in function blocks in Rust? According to Rust AST
-- (Item -> ...-> Block->ItemStmt->Item) and Python AST (Statement-> Fun -> Suite -> Statement)
-- 'recursion' occurs in both. However while in Rust this only seems to 
-- include definitions/imports, in python also e.g. 'For'- loops are top-level statements.
-- Can't I have e.g. function definitions in a function and if so..Why?
    convertExpr Py.Import{import_items=items} = undefined
    convertExpr Py.FromImport{from_module= mod, from_items=items} = undefined
    --TODO: Handle Else_Block
    convertExpr whileE@(Py.While cond do_block else_block annE) = do
        cond' <- convertExpr cond
        block' <- convertExpr do_block
        else_block' <- convertExpr else_block
        --Question: Can we/Shoudl we be sure, that annotation is always a ScrSpan at this point 
        -- and so use location in the file as reference name?
        let loopRef = makeLoopRef "while_loop_body" annE
        let loopLambdaRef = "while_loop_body"
        let recur = IfE
                        cond'
                        (VarE loopLambdaRef `AppE` [])
                        $ LitE UnitLit
        return $
            LetE
                (VarP loopLambdaRef)
                (LamE [] $ StmtE block' recur)
                recur
    convertExpr forE@(Py.For targets generator body elseBlock annot) = do
        patterns <- mapM convertPat targets
        generator' <- convertExpr generator
        body' <- convertExpr body
        return $
            MapE
                (LamE patterns body')
                generator'
    convertExpr asyncFor@(Py.AsyncFor stmt annot) = undefined
    convertExpr classDef@(Py.Class cName cArgs cBody annot) = undefined
    convertExpr ifElifElse@(Py.Conditional condsAndBodys elseBlock annot) = undefined
    convertExpr assign@(Py.Assign targets exor annot) = undefined
    convertExpr augmAs@(Py.AugmentedAssign target operation expr annot) = undefined
    convertExpr annotAs@(Py.AnnotatedAssign targetAnnot target expr stmtAnnot) = undefined
    convertExpr dec@(Py.Decorated decorators funOrClass annot) = undefined
    convertExpr ret@(Py.Return optReturn annot) = undefined
    convertExpr try@(Py.Try block excepts elseBlock finallyBlock annot)= undefined
    convertExpr raise@(Py.Raise raiseExor annot) = undefined
    convertExpr with@(Py.With contextTuples block annot) = undefined
    convertExpr asyncWith@(Py.AsyncWith stmt annot) = undefined
    convertExpr (Py.Pass annot) = undefined
    convertExpr (Py.Break annot) = undefined
    convertExpr (Py.Continue annot) = undefined
    convertExpr (Py.Delete deleteExprs annot) = undefined
    convertExpr (Py.StmtExpr expr annot) = undefined
    convertExpr e@(Py.Global globalVars annot) = unsupError "global keyword" e
    convertExpr e@(Py.NonLocal nonlocalVars annot) = unsupError "nonlocal keyword" e
    convertExpr e@(Py.Assert assertions annot) = unsupError "assertions" e
    convertExpr e@(Py.Print hasChevron args isCOmmaTrailed annot) = py2Error e
    convertExpr e@(Py.Exec expr optionalGlobalsLocals annot) = unsupError "exec statements" e


makeLoopRef :: (Show a) => String -> a -> String
-- TODO: CAse we like the idea, propagate ScrSpan => a throug all fknts and
-- produce name based on coords here. 
-- Alternative make case distinction here if we may want to change the annotations
makeLoopRef loopKind loc = loopKind ++ "_"



instance (Show a) => ConvertExpr (Py.Suite a) where
    convertExpr statements = undefined


instance (Show a) => ConvertExpr (Py.Op a) where
    convertExpr Py.Plus{} = toExpr "+"
    convertExpr Py.Minus{} = toExpr "-"
    convertExpr Py.Multiply{} = toExpr "*"
    convertExpr Py.Divide{} = toExpr "/"
    convertExpr Py.FloorDivide{} = toExpr "//"
    convertExpr Py.Modulo{} = toExpr "%"
    convertExpr Py.Exponent{} = toExpr "**"
    convertExpr Py.MatrixMult{} = toExpr "@"

    --Question: Do I need spacing for word-formed Epxrs ?
    convertExpr Py.And{} = toExpr "and"
    convertExpr Py.Or{}  = toExpr "or"
    convertExpr Py.Not{}  = toExpr "not"
    convertExpr Py.In{} = toExpr "in"
    convertExpr Py.Is{} = toExpr "is"
    convertExpr Py.IsNot{} = toExpr "is not"
    convertExpr Py.NotIn{} = toExpr "not in"

    convertExpr Py.LessThan{} = toExpr "<"
    convertExpr Py.GreaterThan{} = toExpr ">"
    convertExpr Py.Equality{}  = toExpr "=="
    convertExpr Py.GreaterThanEquals{} = toExpr ">="
    convertExpr Py.LessThanEquals{} = toExpr "<="
    convertExpr Py.NotEquals{} = toExpr "!="

    convertExpr Py.BinaryAnd{} = toExpr "&"
    convertExpr Py.BinaryOr{} = toExpr "|"
    convertExpr Py.Xor{} = toExpr "^"
    convertExpr Py.ShiftLeft{} = toExpr "<<"
    convertExpr Py.ShiftRight{} = toExpr ">>"
    convertExpr Py.Invert{} = toExpr "~"

toExpr :: Monad m => Binding -> m (FrLang.Expr ty)
{-- Note: Turns given string representation into literal expression representig an untyped, 
'unscoped' (the empty list in as Binding argument) function reference 
-- Question: why untyped ? --}
toExpr string_repr = return $
                        LitE $ FunRefLit $
                        FunRef (QualifiedBinding (makeThrow []) string_repr) Nothing Untyped

unsupError text expr = throwError $ "Currently we do not support "<> text <>" used in: "<> show expr

--TODO: can this be my responsibility in any way or redirect to bjpop@csse.unimelb.edu.au here ?
py2Error expr = throwError $ "For whatever reason you managed to get the exclusively version 2 expression "
                                <> show expr <> " through the python3 parser of language-python."

emptySpan = SpanEmpty