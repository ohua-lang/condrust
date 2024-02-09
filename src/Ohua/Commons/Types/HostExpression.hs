{-# LANGUAGE
    ConstraintKinds
    , StandaloneDeriving
#-}

module Ohua.Commons.Types.HostExpression where

import Universum

data HostExpression embExpr where
    HostExpression :: (Eq embExpr, Show embExpr) => embExpr  -> HostExpression embExpr

deriving instance Show (HostExpression embExpr)
deriving instance Eq (HostExpression embExpr)
-- FIXME: This doesn't make a lot of sense. So in case we need an actual hashing we'll need to enforce 
-- hashable host expressions
instance Hashable (HostExpression embExpr) where
    hashWithSalt s he = s