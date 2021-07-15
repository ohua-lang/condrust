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
            extractNs (Py.Module [statements]) = do
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
                                    Just . (\e -> Algo (toBinding fun) e fun) <$> extractAlgo fun
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

loadTypes :: CompM m => Language 'Python -> 
                Module -> 
                PythonNamespace -> 
                m PythonNamespace
loadTypes lang (Module filepath pymodule) ohuaNS = do
    -- Note to me: '^.' is a lens operator to focus 'algos' in ohuaNs
    filesAndPaths <- concat <$> mapM funsForAlgo (ohuaNS^.algos)
    let filesAndPaths' = map (first convertOwn) filesAndPaths
    fun_types <- load $ concatMap fst filesAndPaths'
    types' <- HM.fromList <$> mapM (verifyAndRegister typez) filesAndPaths'
    updateExprs ohuaNS (transformM (assignTypes types'))
    where
        assignTypes :: CompM m => FunTypes -> FrLang.Expr (PythonArgType SrcSpan) -> m (FrLang.Expr (PythonArgType SrcSpan))
        --TODO: 
        assignTypes typez = undefined 

        convertOwn :: [NSRef] -> [NSRef]
        convertOwn [] = [filePathToNsRef filepath]
        convertOwn n = n

        funsForAlgo :: CompM m => Algo (FrLang.Expr (PythonArgType SrcSpan)) (Py.Statement SrcSpan) -> m [([NSRef], QualifiedBinding)]
        funsForAlgo (Algo _name code annotation) = do
            return []
            