module Ohua.Frontend.Types where

import Ohua.Prelude

import Ohua.Frontend.Lang
import System.FilePath
import qualified Data.HashMap.Lazy as HM

class Integration lang where
    frontend :: CompM m => FilePath -> lang -> m (lang, Namespace Expr)


type LanguageFileSuffix = Text
type CompilationScope = HM.HashMap NSRef LanguageFileSuffix

-- | This registers all algos used in a given namespace with their qualified names.
type NamespaceRegistry = HM.HashMap QualifiedBinding Expr
