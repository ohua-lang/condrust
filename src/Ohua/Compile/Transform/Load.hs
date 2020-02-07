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

module Ohua.Compile.Transform.Load (load) where

import Ohua.Prelude

import Ohua.Frontend.Lang as FrLang
import Ohua.Compile.Types
import Ohua.Integration.Langs (getParser, definedLangs)
import Ohua.Parser.Common as P
import Ohua.Compile.Util (toFilePath)

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import System.FilePath as Path ((<.>), takeExtension)
import System.Directory (doesFileExist)

-- | Loads the content of an algo file and parses its contents into a (parser-representation) namespace.
-- To prevent namespace clashes, the algo (foo) is always registered as:
-- > some/other/ns:foo => expr
loadModule :: CompM m => FilePath -> m P.Namespace
loadModule fileName = do
    ns <- getParser (toText $ takeExtension fileName) <$> liftIO (L.readFile fileName)
    logDebugN $ "Raw parse result for " <> show fileName
    logDebugN $ "<Pretty printing not implemented for frontend lang yet>" -- quickRender ns
    logDebugN "With annotations:"
    logDebugN $ show $ map (^. P.algoTyAnn) $ ns ^. P.algos
    pure ns

verify :: CompM m => CompilationScope -> P.Namespace -> m ()
verify compScope ns = do
    mapM_
        (\imp -> if HM.member (imp^.nsRef) compScope 
                 then return ()
                 else throwError $ "No such module registered: " <> show (imp^.nsRef))
        $ ns^.imports
    return ()

-- | This function loads all dependencies into the current namespace.
--   We currently use a very simple but easily maintainable approach:
--   Just load all algorithms that exist in the project scope. By using
--   a lazy hashmap, this should only load the once actually needed.
loadDeps :: CompM m => CompilationScope -> P.Namespace -> m NamespaceRegistry
loadDeps scope currentNs = do
    let registry' = registerAlgos HM.empty currentNs
    modules <- mapM (loadModule . toFilePath) $ HM.toList scope
    let registry'' = foldl registerAlgos registry' modules
    return registry''
    where
        registerAlgos :: NamespaceRegistry -> P.Namespace -> NamespaceRegistry
        registerAlgos registry ns = 
            foldl 
                (\reg algo -> 
                    HM.insert 
                        (QualifiedBinding (ns^.nsName) (algo^.algoName))
                        (algo^.algoCode) 
                        reg)
                registry
                $ ns^.algos

load :: CompM m => CompilationScope -> FilePath -> m (P.Namespace, NamespaceRegistry)
load scope inFile = do
    ns       <- loadModule inFile
    verify scope ns
    registry <- loadDeps scope ns
    return (ns, registry)
