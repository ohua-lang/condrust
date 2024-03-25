{-# LANGUAGE
    ConstraintKinds
    , StandaloneDeriving
#-}

module Ohua.Commons.Types.HostAnnotation where

import Universum

data HostAnnotation anno where
    HostAnnotation :: (Eq anno, Show anno) => anno  -> HostAnnotation anno

deriving instance Show ( HostAnnotation anno )
deriving instance Eq ( HostAnnotation anno )
-- FIXME: This doesn't make a lot of sense. So in case we need an actual hashing we'll need to enforce 
-- hashable host expressions
instance Hashable ( HostAnnotation anno ) where
    hashWithSalt s he = s
