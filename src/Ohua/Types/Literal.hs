{-# LANGUAGE
  DataKinds
#-}

module Ohua.Types.Literal where

import Universum
import Ohua.Types.Bindings (Binding)
import Ohua.Types.Reference ( FunRef(..), OhuaType(..), Heq(..), InternalType(..), Resolution(..), getRefType)
import Ohua.Types.HostExpression (HostExpression(..))

-- | Literals of kinds we expect any host language to support

data Lit lang ty res
    = NumericLit !Integer -- ^ an integer literal
    | BoolLit Bool -- ^ a boolean literal
    | UnitLit -- ^ aka @()@
    | StringLit String
    | HostLit (HostExpression lang) (OhuaType ty 'Resolved) 
    | EnvRefLit Binding (OhuaType ty res) -- ^ a variable bound by the outermost lambda that we compile
    | FunRefLit (FunRef ty res) -- ^ Reference to an external function
    deriving (Show, Generic)

instance Eq (Lit lang ty res) where
  (NumericLit i1) == (NumericLit i2) = i1 == i2
  (BoolLit b1) == (BoolLit b2) = b1 == b2
  UnitLit == UnitLit = True
  (StringLit s1) == (StringLit s2) = s1 == s2
  (HostLit hl1 ty1) == (HostLit hl2 ty2) = hl1 == hl2
  (EnvRefLit b1 ty1) == (EnvRefLit b2 ty2) = b1 == b2 && heq ty1 ty2
  (FunRefLit r1) == (FunRefLit r2) = r1 == r2
  _ == _ = False

getLitType :: Lit lang ty Resolved -> OhuaType ty Resolved 
getLitType (NumericLit _) = IType TypeNat
getLitType (BoolLit _)    = IType TypeBool
getLitType UnitLit        = IType TypeUnit
getLitType (StringLit _)  = IType TypeString
getLitType (HostLit _hl ty)   = ty
getLitType (EnvRefLit _b vTy) = vTy
getLitType (FunRefLit fRef)   = FType $ getRefType fRef

instance Hashable (Lit lang ty res)
