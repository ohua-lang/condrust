module Ohua.Frontend.Transform.Load (load) where

import Ohua.Prelude

import Ohua.Frontend.Lang
import Ohua.Frontend.Types

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import System.FilePath as Path ((<.>), takeExtension, joinPath)
import System.Directory (doesFileExist)
import System.FilePath.Posix (addExtension)


nsToFilePath :: NSRef -> FilePath
nsToFilePath = joinPath . map (T.unpack . unwrap) . unwrap 

toFilePath :: (NSRef, Text) -> FilePath
toFilePath (nsRef, suffix) = addExtension (nsToFilePath nsRef) $ T.unpack suffix

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
loadDeps :: (CompM m, Integration lang) => lang -> CompilationScope -> Namespace Expr -> m NamespaceRegistry
loadDeps lang scope currentNs = do
    let registry' = registerAlgos HM.empty currentNs
    modules <- mapM (\path -> snd <$> loadNs lang (toFilePath path)) $ HM.toList scope
    let registry'' = foldl registerAlgos registry' modules
    return registry''
    where
        registerAlgos :: NamespaceRegistry -> Namespace Expr -> NamespaceRegistry
        registerAlgos registry ns = 
            foldl 
                (\reg algo -> 
                    HM.insert 
                        (QualifiedBinding (ns^.nsName) (algo^.algoName))
                        (algo^.algoCode) 
                        reg)
                registry
                $ ns^.algos

load :: (CompM m, Integration lang) => lang -> CompilationScope -> FilePath -> m (NS lang, (Namespace Expr, NamespaceRegistry))
load lang scope inFile = do
    logDebugN $ "Loading module: " <> show inFile <> "..."
    (ctxt, ns) <- loadNs lang inFile
    logDebugN $ "Loaded ns: " <> show ns
    -- verify scope ns
    logDebugN "Loading dependencies ..."
    registry <- loadDeps lang scope ns
    logDebugN "compiled ns successfully."
    return (ctxt, (ns, registry))
