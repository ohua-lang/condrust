module Ohua.Types.Unresolved.Literal
  (module Ohua.Types.Common.Literal)
where

import Ohua.Types.Common.Literal

{-
import Universum
import Ohua.Types.Bindings (Binding)
import Ohua.Types.Unresolved.Reference ( FunRef(..), OhuaType(..) )

-- | Literals of kinds we expect any host language to support
data Lit ty s
    = NumericLit !Integer -- ^ an integer literal
    | BoolLit Bool -- ^ a boolean literal
    | UnitLit -- ^ aka @()@
    | StringLit String
    -- | forall lit. backend -> Hostlit lit (VarType ty) TODO we would need to provide a closure to make sure that the
    --                                                        type stays usable in the backend.
    | EnvRefLit Binding (OhuaType ty s) -- ^ a variable bound by the outermost lambda that we compile
    | FunRefLit (FunRef ty s) -- ^ Reference to an external function
    deriving (Show, Generic)

instance Heq (Lit ty s1) (Lit ty s2) where
  heq (NumericLit i1) (NumericLit i2) = i1 == i2
  heq (BoolLit b1) (BoolLit b2) = b1 == b2
  heq UnitLit UnitLit = True
  heq (StringLit s1) (StringLit s2) = s1 == s2
  heq (EnvRefLit b1 ty1) (EnvRefLit b2 ty2) = b1 == b2 && heq ty1 ty2
  heq (FunRef r1) (FunRef r2) = heq r1 r2
  heq _ _ = False

-- instance Hashable (Lit ty)

getLitType :: Lit ty -> OhuaType ty s
getLitType (NumericLit _) = TypeNat
getLitType (BoolLit _)    = TypeBool
getLitType UnitLit        = TypeUnit
getLitType (StringLit _)  = TypeString
getLitType (EnvRefLit _b vTy)  = vTy
getLitType (FunRefLit (FunRef _ _ vty))  = TypeFunction vty
-}
