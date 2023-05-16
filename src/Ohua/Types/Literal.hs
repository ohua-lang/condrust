module Ohua.Types.Literal where

import Universum
import Ohua.Types.Reference ( FunRef(..), Binding, VarType(..) )

-- | Literals of kinds we expect any host language to support
data Lit ty
    = NumericLit !Integer -- ^ an integer literal
    | BoolLit Bool -- ^ a boolean literal
    | UnitLit -- ^ aka @()@
    | StringLit String
    -- Reminder: To support any kind of host literals we could have:
    -- We would need to require however, that any of the possible literals can be turned  into/recovered from a string
    --  | Hostlit String (VarType ty)
    | EnvRefLit Binding -- ^ a variable bound by the outermost lambda that we compile
    | FunRefLit (FunRef ty) -- ^ Reference to an external function
    deriving (Show, Eq, Generic)

instance Hashable (Lit ty)

getVarType :: Lit ty -> Maybe (VarType ty)
getVarType (NumericLit _) = Just TypeNat
getVarType (BoolLit _)    = Just TypeBool
getVarType UnitLit        = Just TypeUnit
getVarType (StringLit _)  = Nothing
getVarType (EnvRefLit _)  = Nothing
getVarType (FunRefLit _)  = Nothing
