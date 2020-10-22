{-# LANGUAGE ScopedTypeVariables #-}
module Ohua.Frontend.Transform.Load (load) where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Lang
import Ohua.Frontend.Types

import qualified Data.HashMap.Strict as HM


-- FIXME This wants to check whether an import is registered in the global list. 
--       But this can only be performed the other way around because other imports may refer to functions instead of algos.
--       So what is this supposed to verify at all???
-- verify :: CompM m => CompilationScope -> Namespace FrLang.Expr -> m ()
-- verify compScope ns =
--     mapM_
--         (\imp -> if HM.member (imp^.nsRef) compScope 
--                  then return ()
--                  else throwError $ "No such module registered: " <> show (imp^.nsRef))
--         $ ns^.imports


-- | This function loads all dependencies into the current namespace.
--   We currently use a very simple but easily maintainable approach:
--   Just load all algorithms that exist in the project scope. By using
--   a lazy hashmap, this should only load the algo once actually needed.
loadDeps :: forall m lang.(CompM m, Integration lang) => lang -> CompilationScope -> Namespace (Expr (Type lang)) -> m (NamespaceRegistry (Type lang))
loadDeps lang scope currentNs = do
    let registry' = registerAlgos HM.empty currentNs
    modules <- mapM (\path -> snd <$> loadNs lang (toFilePath path)) $ HM.toList scope
    let registry'' = foldl registerAlgos registry' modules
    return registry''
    where
        registerAlgos :: NamespaceRegistry ty -> Namespace (Expr ty) -> NamespaceRegistry ty
        registerAlgos registry ns = 
            foldl 
                (\reg algo -> 
                    HM.insert 
                        (QualifiedBinding (ns^.nsName) (algo^.algoName))
                        (algo^.algoCode) 
                        reg)
                registry
                $ ns^.algos

load :: forall m lang.(CompM m, Integration lang) => lang -> CompilationScope -> FilePath -> m (NS lang, (Namespace (Expr (Type lang)), NamespaceRegistry (Type lang)))
load lang scope inFile = do
    logDebugN $ "Loading module: " <> show inFile <> "..."
    (ctxt, ns) <- loadNs lang inFile
    logDebugN $ "Loaded ns: " <> show (ns^.nsName)
    -- verify scope ns
    logDebugN "Loading dependencies ..."
    registry <- loadDeps lang scope ns
    logDebugN "compiled ns successfully."
    return (ctxt, (ns, registry))
