module Ohua.Frontend.Types where

import Ohua.Prelude

import Ohua.Frontend.Lang
import qualified Data.HashMap.Lazy as HM

-- TODO: What is the proper type constellation here, so that the first parameter of
--       both functions in the interface become unnecessary/obsolete.

class Integration lang where
    type NS lang :: *
    type Types lang :: *

    loadNs :: CompM m => lang -> FilePath -> m (NS lang, Namespace Expr)
    loadTypes :: CompM m => lang -> NS lang -> Namespace Expr -> m (Types lang)

type LanguageFileSuffix = Text
type CompilationScope = HM.HashMap NSRef LanguageFileSuffix

-- | This registers all algos used in a given namespace with their qualified names.
type NamespaceRegistry = HM.HashMap QualifiedBinding Expr
