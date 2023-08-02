{-# LANGUAGE
    TemplateHaskell
    , GeneralizedNewtypeDeriving
    , DeriveLift
    , MultiWayIf
    , ConstraintKinds
    , QuantifiedConstraints
    , UndecidableInstances
    , DataKinds
    , TypeFamilies
    , StandaloneDeriving
    , ScopedTypeVariables
    , TypeApplications
    , TypeOperators
    , PolyKinds
    , StandaloneKindSignatures
    , AllowAmbiguousTypes
    , InstanceSigs
#-}

module Ohua.Types.Common.Reference where

import Universum hiding (Nat, toList)

import Data.Kind
import Data.Type.Equality

import qualified Data.List.NonEmpty as NE (length, zip, (<|))

import GHC.Generics
import Control.Lens.TH
import Control.Lens.Plated
import Instances.TH.Lift ()
import Prettyprinter (Pretty(..))
import GHC.Exts (IsList(..))
import qualified Data.Text as T
import Control.Monad.Error.Class (MonadError, throwError)
import Language.Haskell.TH.Syntax as TH (Lift(..))
import Language.Haskell.TH.Lift (makeLift)

import System.FilePath.Posix (addExtension)
import System.FilePath as Path (joinPath)


import Ohua.Util
import Ohua.LensClasses
import Ohua.Types.Error
import Ohua.Types.Make
import Ohua.Types.Classes
import Ohua.Types.Unit (Unit)
import Ohua.Types.Bindings
import Ohua.Types.HostTypes

import qualified Text.Show


-- | Internal type representations. While Type and TupleTy capture types from the
--   host language, the literal types TypeNat and TypeBool etc. are used internaly to construct nodes
--   They must be mapped to the according types of the host language in the backend.

-- | Heterogeneous equality taken from here:
-- https://github.com/sweirich/dth/blob/master/nested-datatypes/Nested.lhs
class Heq a b where
  heq :: a -> b -> Bool

data Resolution = Unresolved | Resolved

type OhuaType :: Type -> Resolution -> Type
data OhuaType ty s where
  HType :: HostType ty -> Maybe (InternalType ty s) -> OhuaType ty s
  -- TUple and Function types can capture both resolved an unresolved host types
  -- ToDo: Check if we need to introduce another layer of typing 
  TType :: NonEmpty (OhuaType ty s)                 -> OhuaType ty s
  FType :: FunType ty s                             -> OhuaType ty s
  IType :: InternalType ty Resolved                 -> OhuaType ty Resolved
  TStar ::                                             OhuaType ty Unresolved

-- We need hashes for HM 
instance Hashable (OhuaType ty res) where
  hashWithSalt s oty = s

-- We need that for hashing
instance Eq (OhuaType ty res) where
  (==) = heq 

deriving instance Show (OhuaType ty s)
instance Heq (OhuaType ty s1) (OhuaType ty s2) where
  heq (HType ty1 _) (HType ty2 _) = ty1 == ty2
  heq (IType ty1) (IType ty2) = heq ty1 ty2
  heq TStar TStar = True
  heq _ _ = False

type InternalType :: Type -> Resolution -> Type
data InternalType ty s where
  TypeNat :: InternalType ty s
  TypeBool :: InternalType ty s
  TypeUnit :: InternalType ty s
  TypeString :: InternalType ty s
  TypeList :: OhuaType ty s -> InternalType ty s
  -- TupleTy :: NonEmpty (OhuaType ty s) -> InternalType ty s
  -- TypeFunction :: FunType ty s -> InternalType ty s

-- We need hashes for HM 
instance Hashable (InternalType ty res) where
  hashWithSalt s ity = s

-- We need that for hashing
instance Eq (InternalType ty res) where
  (==) = heq 

deriving instance Show (InternalType ty s)
instance Heq (InternalType ty s1) (InternalType ty s2) where
  heq TypeNat TypeNat = True
  heq TypeBool TypeBool = True
  heq TypeUnit TypeUnit = True
  heq TypeString TypeString = True
  heq (TypeList ty1) (TypeList ty2) = heq ty1 ty2
  --heq (TupleTy tys1) (TupleTy tys2) =
    --NE.length tys1 == NE.length tys2 &&
    --all (uncurry heq) (NE.zip tys1 tys2)
  -- heq (TypeFunction ty1) (TypeFunction ty2) = heq ty1 ty2
  heq _ _ = False

type FunType :: Type -> Resolution -> Type
data FunType ty s where
     FunType :: NonEmpty (OhuaType ty s) -> OhuaType ty s -> FunType ty s
     -- FIXME This is not properly defined.
     -- STFunType s [] t
     -- versus
     -- STFunType s [TypeUnit] t
     -- Yet formally, STFunType s [] t :: S -> T and that is ok.
     STFunType :: OhuaType ty s -> [OhuaType ty s] -> OhuaType ty s -> FunType ty s

deriving instance Show (FunType ty s)

-- We need hashes for HM 
instance Hashable (FunType ty res) where
    hashWithSalt s ft = s 

-- We need that for hashing
instance Eq (FunType ty res) where
  (==) = heq 


instance Heq (FunType ty s1) (FunType ty s2) where
  heq (FunType args1 res1) (FunType args2 res2) =
    NE.length args1 == NE.length args2 &&
    all (uncurry heq) (NE.zip args1 args2) &&
    heq res1 res2
  heq (STFunType s1 args1 res1) (STFunType s2 args2 res2) =
    heq s1 s2 &&
    length args1 == length args2 &&
    all (uncurry heq) (zip args1 args2) &&
    heq res1 res2
  heq _ _ = False


resToUnres :: OhuaType ty Resolved -> Maybe (OhuaType ty Unresolved)
-- HTpye gets it's resoltuion from Internal Type, which can only be resolved at this point so I can't pass it back
resToUnres (HType hty miTy)   = Just $ HType hty Nothing
resToUnres (TType tys) = case mapM resToUnres tys of
      Just rTys -> Just $ TType rTys
      Nothing -> Nothing
-- ToDO: Can/Should we unreoslve function types?
resToUnres (FType fTy)  = Nothing
resToUnres (IType _ )         = Nothing

unresToRes :: OhuaType ty Unresolved -> Maybe (OhuaType ty Resolved)
-- HTpye gets it's resolution from Internal Type, which can only be resolved at this point so I can't pass it back
unresToRes (HType hty _miTy)   = Just $ HType hty Nothing
unresToRes (TType tys) = case mapM unresToRes tys of
      Just rTys -> Just $ TType rTys
      Nothing -> Nothing
-- ToDO: Can/Should we unreoslve function types?
unresToRes (FType fty) = Nothing
unresToRes TStar              = Nothing 


isUnresolved :: OhuaType ty Unresolved -> Bool
isUnresolved t = case t of
         (HType _ _) -> True
         (TType tys) -> any isUnresolved tys
         (FType (FunType ins out)) -> any isUnresolved (out NE.<| ins)
         (FType (STFunType state ins out)) -> any isUnresolved (state NE.<| (out :| ins ))
         --(IType _) -> False -- Error/Warning: inaccesible code
         TStar -> True


--------------------------------------------------------------
--               Representation of Variables
--------------------------------------------------------------

-- | A typed Binding
data TypedBinding ty = TBind Binding (OhuaType ty Resolved) deriving (Generic)

deriving instance Show (TypedBinding ty)

instance Hashable (TypedBinding ty) where
    hashWithSalt s (TBind b _ty) = hashWithSalt s b

instance Ord (TypedBinding ty) where
    (TBind b1 _ty1) <= (TBind b2 _ty2) = b1 <= b2

-- FIXME: This is just a hack until we get everything typed correctly
-- currently 'reduceLambdas' in ALang Passes will loop forever if types dont match
-- As long as we can not make sure, that every binding and usage side is correctly typed, tranformations, in particular the ones that determine if something is used
-- should only check if something with the same name, not necesarily the same type annotation is used.
instance Eq (TypedBinding ty) where
    (TBind b1 _ty1) == (TBind b2 _ty2) = b1 == b2

--------------------------------------------------------------
--             Representation of Functions
--------------------------------------------------------------

{-
-- Actually we can only do it this way, i.e. without involving the argument type
-- at each call side because we assume that either generics are not allowed or
-- are also allowed in the backend such that we can consider a generic return type 
-- as fully resolved.
getReturnType :: FunType ty -> VarType ty
getReturnType (FunType _ins out) = out
getReturnType (STFunType _s _ins out) = out

pureArgTypes :: FunType ty -> NonEmpty (VarType ty)
pureArgTypes (FunType ins _out) = ins
pureArgTypes (STFunType s ins _out) = ins

stateArgTypes :: FunType ty -> Maybe (VarType ty)
stateArgTypes (FunType _ins _out) = Nothing
stateArgTypes (STFunType s _ins _out) = Just s

setReturnType :: VarType ty -> FunType ty -> FunType ty
setReturnType ty (FunType ins out) = FunType ins ty
setReturnType ty (STFunType s ins out) = STFunType s ins ty

setFunType :: NonEmpty (VarType ty) -> VarType ty -> FunType ty -> FunType ty
setFunType intys outty (FunType _i _out) = FunType intys outty
setFunType intys outty (STFunType s _ins _out) = STFunType s intys outty 
-}

type FunRef :: Type -> Resolution -> Type
data FunRef ty s where
    FunRef :: QualifiedBinding -> Maybe FnId -> FunType ty s -> FunRef ty s

-- getRefType (FunRef _q _i funTy) = funTy
-- getRefReturnType (FunRef _q _i funTy) = getReturnType funTy

--------------------------------------------------------------
--                           Instances
--------------------------------------------------------------

deriving instance Show (FunRef ty s)
instance Eq (FunRef ty s) where
  (FunRef qb1 _ ty1) == (FunRef qb2 _ ty2) = qb1 == qb2 && heq ty1 ty2