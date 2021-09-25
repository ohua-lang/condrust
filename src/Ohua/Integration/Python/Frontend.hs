{-# LANGUAGE InstanceSigs#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ohua.Integration.Python.Frontend where

import Ohua.Prelude

import Ohua.Frontend.Lang as FrLang
import Ohua.Frontend.Types
import Ohua.Frontend.PPrint ()
import Ohua.Frontend.Convert

import Ohua.Integration.Lang
import Ohua.Integration.Python.NewFrontend
    ( viaSubToIR, viaSubToIRStmt, viaSubToIRSuite)
import Ohua.Integration.Python.Types
import Ohua.Integration.Python.Util
import Ohua.Integration.Python.TypeExtraction
import qualified Ohua.Integration.Python.Frontend.Subset as Sub

import qualified Language.Python.Common.AST as Py
import Language.Python.Common (SrcSpan (SpanEmpty))

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

type PythonNamespace = Namespace (FrLang.Expr PythonArgType) (Py.Statement SrcSpan)

instance Integration (Language 'Python) where
    type NS (Language 'Python) = Module
    type Type (Language 'Python) =  PythonArgType
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

-- TODO: Important -> Reassignments (x += 1, x = x + 1)
-- Note: Produces namespace later refered to with/required as 'ohuaNS^.algos' and 'ohuaNS^.imports'
-- Todo: 1. PythonSubset als data anlegen
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
                                    imp@Py.Import{import_items= impts} -> Just <$> extractImports impts
                                    frImp@Py.FromImport{from_module = modName,
                                                        from_items= items} -> Just <$> extractRelativeImports modName items
                                    _ -> return Nothing)
                                statements
                    algos <- catMaybes <$>
                            mapM
                                (\case
                                    fun@Py.Fun{} ->
                                        Just . (\e -> Algo (toBinding$ Py.fun_name fun) e fun) <$> extractAlgo fun
                                    _ -> return Nothing)
                                statements
                    return $ Namespace (filePathToNsRef srcFile) imports algos

                extractAlgo :: CompM m => Py.Statement SrcSpan -> m (FrLang.Expr PythonArgType )
                extractAlgo function = do
                    args' <- mapM convertPat (Py.fun_args function)
                    block' <- convertExpr (Py.fun_body function)
                    return $ LamE args' block'

                extractImports::CompM m => [Py.ImportItem SrcSpan] -> m [Import]
                -- TODO: Normal imports are Glos, imports with an 'as' are Alias
                -- > Full imports are allways Py.RelativeImport
                extractImports [] = throwError "Invalid: Empty import should not have passed the pytho parser"
                extractImports imports  = return $ map globOrAlias imports

                extractRelativeImports::CompM m => Py.ImportRelative SrcSpan -> Py.FromItems SrcSpan -> m [Import]
                extractRelativeImports imp@(Py.ImportRelative numDots mDottedName annot) fromItems =
                    case mDottedName of
                        Just names -> case fromItems of
                            Py.ImportEverything annot -> return [Glob . makeThrow $ toBindings names]
                            -- Question: Objects can also be imported by their 'real binding' or by alias
                            -- Which one should be the 'binding' in the Full Import?
                            -- OOr can we introduce an alias also for Full Imports?
                            Py.FromItems items annot -> return $ map (fullByNameOrAlias names) items
                        -- Question: Can we solve this by resolving the path or will this inevitably cause problems in distrb. scenario?
                        -- TODO: I realy think we need this as I've literally seen absolut import failing in 'the cloud' cause of
                            -- incompatible python paths (or dark magic :-/)
                        Nothing -> throwError  "Currently we do not support relative import paths"

    loadTypes :: CompM m => Language 'Python ->
                    Module ->
                    PythonNamespace ->
                    m PythonNamespace
    -- TODO: Can meanwhile be replaced by id function
    loadTypes lang (Module filepath pymodule) ohuaNS = do
        -- Alles was vor update epressions steht ist dafür da, functionstypen aus deklarartionen (aus versch. Dateien im comilation scope zu popeln)
        -- _> ich hole mir die typen aus den call und kann mir daher den ersten Teil erstmal sparen
        filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNS^.algos)
        let filesAndPaths' = map (first convertOwn) filesAndPaths
        fun_types <- typesFromNS $ concatMap fst filesAndPaths'
        types' <- HM.fromList <$> mapM (verifyAndRegister fun_types) filesAndPaths'
        updateExprs ohuaNS (transformM (assignTypes types'))
        where
            funsForAlgo :: CompM m => Algo (FrLang.Expr PythonArgType) (Py.Statement SrcSpan)
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
                        -> m (QualifiedBinding, FunType PythonArgType)
            verifyAndRegister fun_types ([candidate], qB@(QualifiedBinding _ qBName)) = undefined
            -- TODO: Can this happen and what to do then?
            verifyAndRegister fun_types ( _ , qB@(QualifiedBinding _ qBName)) = undefined

            assignTypes :: CompM m => FunTypes -> FrLang.Expr PythonArgType -> m (FrLang.Expr PythonArgType)
            assignTypes funTypes function = case function of
                (AppE (LitE (FunRefLit (FunRef qBinding funID _))) args) ->
                    case args of
                        -- Note: In Rust this type assignment happens based on the function definitions, while the
                        -- Python integration does this based on function calls right now.
                        -- Therefore contrary to the Rust way, args might be empty here.
                        -- TODO: When I return to type extraction from defintions, make non-empty args an invariant again
                        {- [] -> throwError "Empty call unfilled."
                        --[LitE UnitLit] -> return $ AppE (LitE $ FunRefLit $ FunRef qBinding funID $ FunType $ Left Unit) args-}
                        (a:args') ->
                            return $
                                AppE (LitE $ FunRefLit (FunRef qBinding funID (listofPyType args))) args
                        _ -> return $ AppE (LitE $ FunRefLit $ FunRef qBinding funID $ FunType $ Left Unit) args
                e ->  return e

            listofPyType :: [FrLang.Expr PythonArgType] -> FunType PythonArgType
            listofPyType [] = error "Empty call unfilled."
            listofPyType (a:args') = FunType $ Right $ map (const $ Type PythonObject) (a:|args')


            globs :: [NSRef]
            globs = mapMaybe (\case (Glob n) -> Just n; _ -> Nothing) (ohuaNS^.imports)

fullByNameOrAlias :: Py.DottedName SrcSpan -> Py.FromItem SrcSpan -> Import
fullByNameOrAlias dotted (Py.FromItem  ident Nothing annot) = flip Full (toBinding ident) . makeThrow $ toBindings dotted
-- TODO: What about aliasing fully qualified imports??
fullByNameOrAlias dotted (Py.FromItem  ident (Just alias) annot) = undefined

globOrAlias :: Py.ImportItem SrcSpan -> Import
globOrAlias  (Py.ImportItem dotted Nothing  annot) = Glob . makeThrow $ toBindings dotted
globOrAlias  (Py.ImportItem dotted (Just alias) annot) = flip Alias (toBinding alias) . makeThrow $ toBindings dotted


instance ConvertPat (Py.Parameter SrcSpan) where
    -- Question: There's a FIXME in Rusts argument conversion to attach (type) info 
    -- > why can't we make VarP have an additional Maybe ?  
    convertPat params@Py.Param{param_name=ident} = return $ VarP $ toBinding ident
    -- Question: Variables can be anything anyway. 
    -- We'll have to tread them like objects as they'r only 'frozen at the surface'. 
    -- So just prepend '*'/'**' to their names (to transfer unpacking to backend)? 
    convertPat params@Py.VarArgsPos{param_name=ident} = return $ VarP $ fromString $ "*"++Py.ident_string ident
    convertPat params@Py.VarArgsKeyword{param_name=ident} = return $ VarP $ fromString $ "**"++Py.ident_string ident
    -- Question: I found out what EndPositional acutally is. It might be there and it needs to be mapped to nothing
    -- i.e. not a fail, but realy nothing. How to do this? 
    -- Meanwhile I will just not support it
    convertPat params@Py.EndPositional{} = unsupError "keyword-only markers as arguments" params
    convertPat tplParam@Py.UnPackTuple{} = unsupError " python 2 tuple unpacking parameters (consult PEP 3113)" tplParam

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
instance ConvertPat (Py.Expr SrcSpan) where
    -- Question: why are wilcards not different from normal vars i.e. also a VarP ... Could I deref them anywhere ?
    convertPat Py.Var {var_ident=ident} = return $ VarP $ toBinding ident
    convertPat lst@(Py.List [expr] annot) = convertPat expr
    convertPat lst@(Py.List exprs annot) = do
        patterns <- mapM convertPat exprs
        return $ TupP patterns
    convertPat lst@(Py.Tuple exprs annot) = do
        patterns <- mapM convertPat exprs
        return $ TupP patterns
    -- Question: Rust implementation doesn't support PathP, which I assume to be attribute
    -- assingment (i.e. ~ Py.Dot). Why?
    convertPat dot@(Py.Dot exprs termVar annot) = unsupError "attribute assignment" dot
    -- Question: I assume it's troublesome for some reason to translate this (probably because in haskell 
    -- we do not modify things but return new ones)...Why exactly?
    convertPat subScr@(Py.Subscript subscriptee indexExpr annot) = unsupError "indexed patterns" subScr
    convertPat slice@(Py.SlicedExpr subscriptee slices annot) = unsupError "slice patterns" slice
    convertPat starred@(Py.Starred expr annot) = unsupError "starred expression patterns" starred
    convertPat any = throwError $ "Encountered " <> show any <> " while trying to convert patterns. This is a bug"

instance ConvertExpr (Py.Expr SrcSpan) where
    convertExpr = viaSubToIR 

instance  ConvertExpr (Py.Argument SrcSpan) where
    -- TODO: Support agrs n kwrags
    convertExpr Py.ArgExpr{arg_expr=expr} = convertExpr expr
    convertExpr a@Py.ArgVarArgsPos{arg_expr=expr} = unsupError "*args" a
    convertExpr a@Py.ArgVarArgsKeyword {arg_expr=_arg_expr} = unsupError "**kwargs" a
    convertExpr a@Py.ArgKeyword{arg_keyword= varN, arg_expr= expr} = unsupError "keyword arguments" a


instance  ConvertExpr (Py.Suite SrcSpan) where
    convertExpr = viaSubToIRSuite
    {-
         case statements of
            [] -> throwError "Empty function body. Actually that shouldn't have passed the parser"
            (x:xs) ->
                let last = NE.head $ NE.reverse $ x:|xs
                    heads = NE.tail $ NE.reverse $ x:|xs
                in do
                    convertedLast <- convertLastStmt last
                    foldM
                     (\cont stmt -> (\e -> e cont) <$> convertStmt stmt)
                     convertedLast
                     heads
                where
                    convertStmt :: (CompM m) => Py.Statement SrcSpan -> m (FrLang.Expr ty -> FrLang.Expr ty)
                    convertStmt assign@(Py.Assign [target] expr annot) = do
                        pat' <- convertPat target
                        expr' <- convertExpr expr
                        return $ LetE pat' expr'
                    -- Question: Are assignments to TupP now valid. If not ...se following question.
                    -- TODO/Question: The Error produced by this translation of Py.Assign was
                    -- [Error] Unsupported multiple outputs: Destruct (Direct (DataBinding (Binding "x_0_0")) :| [])
                    -- The reason was that assigning to tuples is obviously a part of the language but functions are not intended to output 
                    -- tuples i.e. no assignments to TupP 
                    -- SO Question 1: Why, what's the prupose of it?
                    -- Question 2: (Rather Todo) How to handle python's unpacking here?
                    -- Note: I can try the following:
                    {-Input:  x,y,z = f()
                     -Output: Let tpl = f() in
                                let x = tpl[0] in
                                    let y = tpl[1] in 
                                        let z = tpl[2] in
                                            .. do stuff ... -}
                    {-
                    convertStmt assign@(Py.Assign targets expr annot) = do
                        pat' <- mapM convertPat targets
                        expr' <- convertExpr expr
                        return $ LetE (TupP pat') expr'
                    -}
                    {-convertStmt augAssign@(Py.AugmentedAssign target operation expr annot) = do
                        -- TODO: I need an outer let here ..
                        -- x += 1
                        Let tmp = x 
                            in let x = tmp + 1
                               in x
                       
                        pat' <- convertPat target
                        let binOp = Py.BinaryOp (toBinOp operation) target expr noSpan
                        expr' <- convertExpr binOp
                        return $ LetE pat' expr'
                     -}
                    -- Question: I understand return statements are not supported in Rust, 
                    -- as they are optional there and might exit functions at different points, right?
                    -- So I'd assume that I should only support them as the last statement in a block right?                 
                    convertStmt stmt = StmtE <$> convertExpr stmt

                    -- Cases for last statement 
                        -- -> either it's a return statement with an expression 
                            -- this should be equivalent to NoSemi in Rust 
                            -- > return the converted expression 
                        -- or it's an empty return statement
                            -- this should be equivalent to having a Semi Statement last in Rust
                            -- except that ther's no statement to translate before -> 
                            -- return LitE UnitLit
                        -- or it's just any statement
                            -- convert the statement and append LitE UnitLit

                    convertLastStmt :: (CompM m) => Py.Statement SrcSpan -> m (FrLang.Expr ty)
                    convertLastStmt ret@(Py.Return maybeExpr annot) =
                        case maybeExpr of
                             Just expr -> convertExpr expr
                             Nothing -> return $ LitE UnitLit
                    convertLastStmt stmt = (\e -> e $ LitE UnitLit) <$> convertStmt stmt
-}

unsupError text expr = throwError $ "Currently we do not support "<> text <>" used in: "<> show expr

--TODO: can this be my responsibility in any way or redirect to bjpop@csse.unimelb.edu.au here ?
py2Error expr = throwError $ "For whatever reason you managed to get the exclusively version 2 expression "
                                <> show expr <> " through the python3 parser of language-python."


toBindings = map toBinding

