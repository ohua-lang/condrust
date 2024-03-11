{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Frontend.Transform.Load (loadAlgosAndImports) where

import Ohua.Commons.Prelude hiding (Type)

import Ohua.Frontend.Lang
import Ohua.Frontend.Types

import qualified Data.HashMap.Strict as HM


-- FIXME This wants to check whether an import is registered in the global list. 
--       But this can only be performed the other way around because other imports may refer to functions instead of algos.
--       So what is this supposed to verify at all???
-- verify :: ErrAndLogM m => CompilationScope -> Namespace FrLang.Expr -> m ()
-- verify compScope ns =
--     mapM_
--         (\imp -> if HM.member (imp^.nsRef) compScope 
--                  then return ()
--                  else throwError $ "No such module registered: " <> show (imp^.nsRef))
--         $ ns^.imports

-- QUESTION: Why this distinction between the first and the other modules? 

-- | This function loads all dependencies into the current namespace.
--   We currently use a very simple but easily maintainable approach:
--   Just load all algorithms that exist in the project scope. By using
--   a lazy hashmap, this should only load the algo once actually needed.
loadDeps :: forall m lang.
    (ErrAndLogM m, Integration lang)
    => lang 
    -> CompilationScope 
    -> Namespace (UnresolvedExpr (EmbExpr lang) (Type lang)) (AlgoSrc lang) (OhuaType (Type lang) 'Resolved) 
    -> m (NamespaceRegistry (EmbExpr lang) (Type lang))
loadDeps lang scope (Namespace _name imps globs algs) = do
    let currentNs = Namespace (makeThrow []) imps globs algs
    let registry' = registerAlgos HM.empty currentNs
    modules <- mapM (\path -> (\(_,scnd,_) -> scnd) <$> loadNs lang (toFilePath path)) $ HM.toList scope
    let registry'' = foldl registerAlgos registry' modules
    return registry''
    where
        registerAlgos :: NamespaceRegistry embExpr annot ty 
            -> Namespace (UnresolvedExpr embExpr annot ty) anno (OhuaType ty 'Resolved) 
            -> NamespaceRegistry embExpr annot ty
        registerAlgos registry aNs =
            foldl
                (\reg algo ->
                    HM.insert
                        (QualifiedBinding (aNs^.nsName) (algo^.algoName))
                        (algo^.algoCode, algo^.algoType)
                        reg)
                registry
                $ aNs^.algos


-- | This function calls the language integration to extract defined functions (algorithms) and imports
--   from the input file. Extracting algorithms means, they are translated to an expression of the
--   frontend IR here. In a second step
--   the same is done for all files in the given scope to build up a registry of algorithms, mapping their names
--   (qualified by the file they come from) to their translated code.
loadAlgosAndImports :: forall m lang.
    (ErrAndLogM m, Integration lang)
    => lang 
    -> CompilationScope 
    -> FilePath
    -> m (  HostModule lang, 
            Namespace (UnresolvedExpr (EmbExpr lang) (Type lang)) (AlgoSrc lang) (OhuaType (Type lang) 'Resolved),
            NamespaceRegistry (EmbExpr lang) (Type lang), 
            HostModule lang)
loadAlgosAndImports  lang scope inFile = do
    -- logDebugN $ "Loading module: " <> show inFile <> "..."
    (ctxt, ns', placeholder) <- loadNs lang inFile
    -- let ns' = Namespace (makeThrow []) imports algos -- references to functions contain an empty ref
    -- logDebugN $ "Loaded ns: " <> show (ns'^.nsName)
    -- verify scope ns
    -- logDebugN "Loading dependencies ..."
    registry <- loadDeps lang scope ns'
    -- logDebugN "compiled ns successfully."
    return (ctxt, ns', registry, placeholder)
