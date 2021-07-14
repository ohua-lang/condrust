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

instance Integration (Language 'Python) where
    type NS (Language 'Python) = Module
    type Type (Language 'Python) =  PythonArgType SrcSpan
    type AlgoSrc (Language 'Python) = Py.Statement SrcSpan

type ConcreteNamespace = Namespace (FrLang.Expr (PythonArgType SrcSpan)) (Py.Statement SrcSpan)

loadNs :: CompM m => Language 'Python -> FilePath -> m (Module, ConcreteNamespace)
loadNs _ srcFile = do
        mod <- liftIO $ load srcFile
        ns <- extractNs mod
        return (Module srcFile mod, ns)
        where
            extractNs :: CompM m => Py.Module SrcSpan -> m ConcreteNamespace
            extractNs statements = do
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


--TODO: Make Language Python an instance of Frontend.Integration
{-
class Integration lang where
    type NS lang :: *
    type Type lang :: *
    type AlgoSrc lang :: *

    loadNs :: CompM m => 
        lang -> FilePath -> m (NS lang, Namespace (Expr (Type lang)) (AlgoSrc lang))
    loadTypes :: CompM m => 
        lang -> NS lang -> Namespace (Expr (Type lang)) (AlgoSrc lang) -> m (Namespace (Expr (Type lang)) (AlgoSrc lang))
-}