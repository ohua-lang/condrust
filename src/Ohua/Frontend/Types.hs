module Ohua.Frontend.Types where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Lang
import qualified Data.HashMap.Lazy as HM

-- TODO: What is the proper type constellation here, so that the first parameter of
--       both functions in the interface become unnecessary/obsolete.
-- ToDo: Might work if we use associated data types 'data HostModule lang' instead of 'type HostModule lang'. See example below 

class (Show (Type lang)) => Integration lang where
    type HostModule lang :: *
    type Type lang :: *
    type AlgoSrc lang :: *
    
    loadNs :: ErrAndLogM m
           => lang
           -> FilePath
           -> m (HostModule lang, Namespace (Expr (Type lang)) (AlgoSrc lang) (Type lang), HostModule lang)

    loadTypes :: ErrAndLogM m
              => lang -> HostModule lang
              ->    Namespace (Expr (Type lang)) (AlgoSrc lang) (Type lang)
              -> m (Namespace (Expr (Type lang)) (AlgoSrc lang) (Type lang))

type LanguageFileSuffix = Text
type CompilationScope = HM.HashMap NSRef LanguageFileSuffix

-- | This registers all algos used in a given namespace with their qualified names.
type NamespaceRegistry ty = HM.HashMap QualifiedBinding (Expr ty)


class Someclass a where
       data SpecialThing1 a:: *
       data SpecialThing2 a:: *

       doStuff:: SpecialThing1 a -> SpecialThing2 a -> a


data Something = This Int | That String

instance Someclass Something where
       data SpecialThing1 Something = Thing1 String
       data SpecialThing2 Something = Thing2 Int
       doStuff (Thing1 str) (Thing2 i) 
              | i < 23 =  This i 
              | otherwise = That str
