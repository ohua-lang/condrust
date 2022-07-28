module Ohua.Types.Literal where

import Universum
import Ohua.Types.Reference ( FunRef(..), Binding )

-- | Literals of kinds we expect any host language to support
data Lit ty
    = NumericLit !Integer -- ^ an integer literal
    | BoolLit Bool -- ^ a boolean literal
    | UnitLit -- ^ aka @()@
    | StringLit String
    | EnvRefLit Binding -- ^ a variable bound by the outermost lambda that we compile
    | FunRefLit (FunRef ty) -- ^ Reference to an external function
    deriving (Show, Eq, Generic)

instance Hashable (Lit ty)
