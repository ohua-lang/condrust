module Ohua.Types.Literal where

import Universum
import Ohua.Types.Reference ( FunRef(..), HostExpr )

-- | Literals of kinds we expect any host language to support
data Lit ty
    = NumericLit !Integer -- ^ an integer literal
    | BoolLit Bool -- ^ a boolean literal
    | UnitLit -- ^ aka @()@
    | EnvRefLit !HostExpr -- ^ A reference to some value from the environment
    | FunRefLit (FunRef ty) -- ^ Reference to an external function
    deriving (Show, Eq, Generic)

-- TODO integrate with above definition
data Unit = Unit

instance Hashable (Lit ty)
