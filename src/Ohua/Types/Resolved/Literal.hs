module Ohua.Types.Resolved.Literal where

import Universum
import Ohua.Types.Bindings (Binding)
import Ohua.Types.Resolved.Reference ( FunRef(..), VarType(..), getRefType )

-- | Literals of kinds we expect any host language to support
data Lit ty
    = NumericLit !Integer -- ^ an integer literal
    | BoolLit Bool -- ^ a boolean literal
    | UnitLit -- ^ aka @()@
    | StringLit String
    | EnvRefLit Binding (VarType ty)-- ^ a variable bound by the outermost lambda that we compile
    | FunRefLit (FunRef ty) -- ^ Reference to an external function
    deriving (Show, Eq, Generic)

instance Hashable (Lit ty)

getLitType :: Lit ty -> VarType ty
getLitType (NumericLit _) = TypeNat
getLitType (BoolLit _)    = TypeBool
getLitType UnitLit        = TypeUnit
getLitType (StringLit _)  = TypeString
getLitType (EnvRefLit _b vTy)  = vTy
getLitType (FunRefLit fRef)  = TypeFunction $ getRefType fRef
