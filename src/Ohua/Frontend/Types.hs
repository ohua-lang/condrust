module Ohua.Frontend.Types where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Lang
import qualified Data.HashMap.Lazy as HM

-- TODO: What is the proper type constellation here, so that the first parameter of
--       both functions in the interface become unnecessary/obsolete.

class Integration lang where
    type NS lang :: *
    type Type lang :: *
    type AlgoSrc lang :: *

    loadNs :: CompM m => 
        lang -> FilePath -> m (NS lang, Namespace (Expr (Type lang)) (AlgoSrc lang))
    loadTypes :: CompM m => 
        lang -> NS lang -> Namespace (Expr (Type lang)) (AlgoSrc lang) -> m (Namespace (Expr (Type lang)) (AlgoSrc lang))

type LanguageFileSuffix = Text
type CompilationScope = HM.HashMap NSRef LanguageFileSuffix

-- | This registers all algos used in a given namespace with their qualified names.
type NamespaceRegistry ty = HM.HashMap QualifiedBinding (Expr ty)
