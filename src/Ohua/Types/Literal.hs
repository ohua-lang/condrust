module Ohua.Types.Literal where

import Universum
import Ohua.Types.Reference ( FunRef(..), Binding, VarType(..), getRefReturnType )

-- | Literals of kinds we expect any host language to support
data Lit ty
    = NumericLit !Integer -- ^ an integer literal
    | BoolLit Bool -- ^ a boolean literal
    | UnitLit -- ^ aka @()@
    | StringLit String
    -- Reminder: To support any kind of host literals we could have:
    -- We would need to require however, that any of the possible literals can be turned  into/recovered from a string
    --  | Hostlit String (VarType ty)
    | EnvRefLit Binding (VarType ty)-- ^ a variable bound by the outermost lambda that we compile
    | FunRefLit (FunRef ty) -- ^ Reference to an external function
    deriving (Show, Eq, Generic)

instance Hashable (Lit ty)

getVarType :: Lit ty -> VarType ty
getVarType (NumericLit _) = TypeNat
getVarType (BoolLit _)    = TypeBool
getVarType UnitLit        = TypeUnit
getVarType (StringLit _)  = TypeString
getVarType (EnvRefLit _b vTy)  = vTy
getVarType (FunRefLit fRef)  = getRefReturnType fRef
