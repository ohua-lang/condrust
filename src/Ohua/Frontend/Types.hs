module Ohua.Frontend.Types where

import Ohua.Prelude hiding (Type)

import Ohua.Frontend.Lang
import Ohua.Frontend.TypeSystem (Delta)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Kind as DK

-- TODO: What is the proper type constellation here, so that the first parameter of
--       both functions in the interface become unnecessary/obsolete.
-- ToDo: Might work if we use associated data types 'data HostModule lang' instead of 'type HostModule lang'. See example below 

class (Show (Type lang), Show (EmbExpr lang)) => Integration lang where
    type HostModule lang :: DK.Type
    type Type lang :: DK.Type
    type EmbExpr lang :: DK.Type
    type AlgoSrc lang :: DK.Type

    loadNs :: ErrAndLogM m
           => lang
           -> FilePath
           -> m (HostModule lang, Namespace (UnresolvedExpr (EmbExpr lang) (Type lang)) (AlgoSrc lang) (OhuaType (Type lang) 'Resolved), HostModule lang)

    loadTypes :: ErrAndLogM m
              => lang -> HostModule lang
              -> Namespace (UnresolvedExpr (EmbExpr lang) (Type lang)) (AlgoSrc lang) (OhuaType (Type lang) 'Resolved)
              -> m (Delta (Type lang) Resolved)

type LanguageFileSuffix = Text
type CompilationScope = HM.HashMap NSRef LanguageFileSuffix

-- | This registers all algos used in a given namespace with their qualified names.
type NamespaceRegistry embExpr ty = HM.HashMap QualifiedBinding (UnresolvedExpr embExpr ty, OhuaType ty 'Resolved)

{-
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
-}
