{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
-- Question: What are these extensions for? 
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
import Language.Python.Common (SrcSpan)

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE


type PythonNamespace = Namespace (FrLang.Expr (PythonArgType SrcSpan)) (Py.Statement SrcSpan)

instance Integration (Language 'Python) where
    type NS (Language 'Python) = Module
    type Type (Language 'Python) =  PythonArgType SrcSpan
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

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
loadTypes lang (Module filepath pymodule) ohuaNS = do
    -- Note to me: '^.' is a lens operator to focus 'algos' in ohuaNs
    filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNS^.algos)
    let filesAndPaths' = map (first convertOwn) filesAndPaths
    fun_types <- typesFromNS $ concatMap fst filesAndPaths'
    types' <- HM.fromList <$> mapM (verifyAndRegister fun_types) filesAndPaths'
    updateExprs ohuaNS (transformM (assignTypes types'))
    where
        funsForAlgo :: CompM m => Algo (FrLang.Expr (PythonArgType SrcSpan)) (Py.Statement SrcSpan)
                -> m [([NSRef], QualifiedBinding)]
        -- TODO: What is it supposed to do -> find out and make it do so
        funsForAlgo (Algo _name code annotation) = do
            return []

        convertOwn :: [NSRef] -> [NSRef]
        convertOwn [] = [filePathToNsRef filepath]
        convertOwn n = n

        typesFromNS :: CompM m => [NSRef] -> m FunTypes
        typesFromNS nsRefs = HM.unions <$> mapM (extractFromFile . toFilePath . (,".py") ) nsRefs

        verifyAndRegister :: CompM m => FunTypes -> ([NSRef], QualifiedBinding)
                    -> m (QualifiedBinding, FunType (PythonArgType SrcSpan))
        verifyAndRegister fun_types ([candidate], qB@(QualifiedBinding _ qBName)) =
            case HM.lookup (QualifiedBinding candidate qBName) fun_types of
                Just t -> return (qB, t)
                Nothing -> throwError $ "Function `" <> show (unwrap qBName) <> "` not found in module `" <> show candidate <> "`. I need these types to generate the code. Please check that the function actually exists."

        verifyAndRegister fun_types (globs', qB@(QualifiedBinding _ qBName)) =
            case mapMaybe ((`HM.lookup` fun_types) . (`QualifiedBinding` qBName)) globs' of
                [] -> throwError $ "Function `" <> show (unwrap qBName) <> "` not found in modules `" <> show globs' <> "`. I need these types to generate the code. Please check that the function actually exists."
                [t] -> return (qB, t)
                _ -> throwError $ "Multiple definitions of function `" <> show (unwrap qBName) <> "` in modules " <> show globs' <> " detected!\nPlease verify again that your code compiles properly by running `rustc`. If the problem persists then please file a bug. (See issue sertel/ohua-frontend#1)"


        assignTypes :: CompM m => FunTypes -> FrLang.Expr (PythonArgType SrcSpan) -> m (FrLang.Expr (PythonArgType SrcSpan))
        --TODO: 
        assignTypes typez = undefined

        globs :: [NSRef]
        -- Question: Glob is an Import that represents 'path that imports all bindings in this path.'
        -- Can I get an example?
        globs = mapMaybe (\case (Glob n) -> Just n; _ -> Nothing) (ohuaNS^.imports)


{-instance ConvertPat ...Turns out, pattern matching is under way \o/ (PEP 634)-}

instance (Show a) => ConvertPat (Py.Parameter a) where
    -- Question: Argument conversion was commented with FIXME to attach (type) info 
    -- > why can't we make VarP have an additional Maybe ?  
    convertPat params@Py.Param{param_name=ident} = return $ VarP $ toBinding ident
    -- Question: Varaibles can be anything anyway. 
    -- We'll have to tread them like objects as they'r only 'frozen at the surface'. 
    -- So just prepend '*'/'**' to their names (to transfer unpacking to backend)? 
    convertPat params@Py.VarArgsPos{param_name=ident} = return $ VarP $ fromString $ "*"++Py.ident_string ident
    convertPat params@Py.VarArgsKeyword{param_name=ident} = return $ VarP $ fromString $ "**"++Py.ident_string ident
    -- Question: Is UnitP the correct way to translate this? I think there has to be some recognizable token for the 
    -- python parser at the backend. Using VarP with a part. name or deducing from the '*'-preceded parameters seems not very 
    -- elegant 
    convertPat params@Py.EndPositional{} = return UnitP 



instance (Show a) => ConvertExpr (Py.Expr a) where
    convertExpr Py.Var {var_ident= ident} = return $ VarE $ toBinding ident
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