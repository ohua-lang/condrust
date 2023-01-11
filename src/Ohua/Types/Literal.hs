module Ohua.Types.Literal where

import Universum
import Ohua.Types.Reference ( FunRef(..), Binding, ArgType(..) )

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

getArgType :: Lit ty -> Maybe (ArgType ty)
getArgType (NumericLit _) = Nothing -- Just TypeNat
getArgType (BoolLit _)    = Just TypeBool
getArgType UnitLit        = Just TypeUnit
getArgType (StringLit _)  = Nothing
getArgType (EnvRefLit _)  = Nothing
getArgType (FunRefLit _)  = Nothing
