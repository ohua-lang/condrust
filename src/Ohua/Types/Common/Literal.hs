{-# LANGUAGE
  DataKinds
#-}

module Ohua.Types.Common.Literal where

import Universum
import Ohua.Types.Bindings (Binding)
import Ohua.Types.Common.Reference ( FunRef(..), OhuaType(..), Heq(..), InternalType(..), Resolution(..), getRefType)

-- | Literals of kinds we expect any host language to support
-- FIXME: Do we need resolved and unresolved literals?
data Lit ty res
    = NumericLit !Integer -- ^ an integer literal
    | BoolLit Bool -- ^ a boolean literal
    | UnitLit -- ^ aka @()@
    | StringLit String
    -- | forall lit. backend -> Hostlit lit (VarType ty) TODO we would need to provide a closure to make sure that the
    --                                                        type stays usable in the backend.
    | EnvRefLit Binding (OhuaType ty res) -- ^ a variable bound by the outermost lambda that we compile
    | FunRefLit (FunRef ty res) -- ^ Reference to an external function
    deriving (Show, Generic)

instance Eq (Lit ty res) where
  (NumericLit i1) == (NumericLit i2) = i1 == i2
  (BoolLit b1) == (BoolLit b2) = b1 == b2
  UnitLit == UnitLit = True
  (StringLit s1) == (StringLit s2) = s1 == s2
  (EnvRefLit b1 ty1) == (EnvRefLit b2 ty2) = b1 == b2 && heq ty1 ty2
  (FunRefLit r1) == (FunRefLit r2) = r1 == r2
  _ == _ = False

getLitType :: Lit ty Resolved -> OhuaType ty Resolved 
getLitType (NumericLit _) = IType TypeNat
getLitType (BoolLit _)    = IType TypeBool
getLitType UnitLit        = IType TypeUnit
getLitType (StringLit _)  = IType TypeString
getLitType (EnvRefLit _b vTy)  = vTy
getLitType (FunRefLit fRef)  = FType $ getRefType fRef

instance Hashable (Lit ty res)



{-
instance Heq (Lit ty s1) (Lit ty s2) where
  heq (NumericLit i1) (NumericLit i2) = i1 == i2
  heq (BoolLit b1) (BoolLit b2) = b1 == b2
  heq UnitLit UnitLit = True
  heq (StringLit s1) (StringLit s2) = s1 == s2
  heq (EnvRefLit b1 ty1) (EnvRefLit b2 ty2) = b1 == b2 && heq ty1 ty2
  heq (FunRefLit r1) (FunRefLit r2) = r1 == r2
  heq _ _ = False
-}

-- instance Hashable (Lit ty)
{-
getLitType :: Lit ty -> OhuaType ty Core
getLitType (NumericLit _) = IType TypeNat
getLitType (BoolLit _)    = IType TypeBool
getLitType UnitLit        = IType TypeUnit
getLitType (StringLit _)  = IType TypeString
getLitType (EnvRefLit _b vTy)  = vTy
getLitType (FunRefLit (FunRef _ _ vty))  = IType $ TypeFunction vty
-}
