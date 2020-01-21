{-|

Module      : $Header$
Description : Loading algorithms from other namespaces into the current one.
Copyright   : (c) Sebastian Ertel 2020. All Rights Reserved.
License     : OtherLicense
Maintainer  : sebastian.ertel@gmail.com
Stability   : experimental
Portability : portable
This source code is licensed under the terms described in the associated LICENSE.TXT file

This "transformation" loads algorithms from other modules, imported via import statements,
as top-level algos into the current namespace.
It is only somewhat a transformation because it does not alter any expression. But this is only the case,
because we chose to represent the list of top-level algos in terms of a hash map. Normally, all would form
one big expression.

The following is the interface that a parser needs to adhere to:
Symbols need to be fully qualified.
That means code like this:
@
import "bla/some"
some.ns.foo()
@
must be turned into
@
import "bla/some/ns" "foo" <-- Qualified Binding
foo()
 ^
 |
  - Qualified Binding (inside a function literal)
@
This makes the rest of the compilation straight forward and disambiuates:
@
state.foo()
@
from
@
foo()
@
in languages such as Go or Java.

-}

module Ohua.Compile.Transform.Load where

import qualified Data.HashMap.Strict as HM
import Ohua.Parser.Common as P

definedLangs :: [(Text, Text, P.Parser)]
definedLangs =
    ( ".go"
    , "Go frontend for the algorithm language",
    parseGo) :
    []

getParser :: Text -> P.Parser
getParser ext
    | Just a <- find ((== ext) . view _1) definedLangs = a ^. _3
    | otherwise =
        error $ "No parser defined for files with extension '" <> ext <> "'"

-- | Loads the content of an algo file and parses its contents into a (parser-representation) namespace.
-- To prevent namespace clashes, the algo (foo) is always registered as:
-- > some/other/ns:foo => expr
readAndParse ::
       (MonadLogger m, MonadIO m) => FileRef -> m P.Namespace
readAndParse filename = do
    ns <-
        let strFname = toString filename
         in getParser (toText $ takeExtension strFname) <$>
            liftIO (L.readFile strFname)
    logDebugN $ "Raw parse result for " <> filename
    logDebugN $ "<Pretty printing not implemented for frontend lang yet>" -- quickRender ns
    logDebugN "With annotations:"
    logDebugN $ show $ map (^. P.algoTyAnn) ns ^. P.algos
    pure ns

findSourceFile :: (MonadError Error m, MonadIO m) => NSRef -> m Text
findSourceFile modname = do
    candidates <-
        filterM (liftIO . doesFileExist) $
        map ((asFile Path.<.>) . toString) extensions
    case candidates of
        [] -> throwError $ "No file found for module " <> show modname
        [f] -> pure $ toText f
        files ->
            throwError $
            "Found multiple files matching " <> show modname <> ": " <>
            show files
  where
    asFile = toString $ T.intercalate "/" $ map unwrap $ unwrap modname
    extensions = map (^. _1) definedLangs

-- | Finds the source from an NSRef, loads it into a (parser-representation) 
--   namespace and resolves it by loading its dependencies
loadModule :: CompM m => NSRef -> m P.Namespace
loadModule = readAndParse =<< findSourceFile

-- | This function recurses until it managed to load all dependencies into 
--   the current namespace.
loadDeps :: CompM m => CompilationScope -> P.Namespace -> m (HM.HashMap Binding Expression)
loadDeps scope currentNs = do
    let registry' = registerAlgosIntoNS HM.empty currentNs
    modules <- forM scope loadModule
    let registry'' = foldl registerAlgos registry' modules
    return registry''
    where
        registerAlgosInNs :: HM.HashMap Binding Expression -> P.Namespace -> HM.HashMap Binding Expression
        registerAlgosInNs registry ns = 
            foldl 
                (\reg algo -> 
                    HM.insert 
                        (QualifiedBinding ns^.name (algo^.algoName))
                        (algo^.algoCode) 
                        reg)
                registry
                $ ns^.algos

load :: CompM m => CompilationScope -> NSRef -> m (P.Namespace, HM.HashMap Binding Expression)
load scope = (loadDeps algoImports) =<< loadModule 
