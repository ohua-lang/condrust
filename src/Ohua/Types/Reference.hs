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

module Ohua.Types.Reference where

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
import qualified GHC.TypeLits as Bool
import Data.Bifoldable (Bifoldable(bifoldMap))
import Data.Maybe (fromMaybe)


-- | Internal type representations. While Type and TType capture types from the
--   host language, the literal types TypeNat and TypeBool etc. are used internaly to construct nodes
--   They must be mapped to the according types of the host language in the backend.

-- | Heterogeneous equality taken from here:
-- https://github.com/sweirich/dth/blob/master/nested-datatypes/Nested.lhs
class Heq a b where
  heq :: a -> b -> Bool

data Resolution = Unresolved | Resolved

type OhuaType :: Type -> Resolution -> Type
data OhuaType ty s where
  -- Why does HostType have a Maybe Internal Type: We usually don't want to convert any types form the
  -- host language to the interanl types. The only exception is, when we need to type check that conditions are really Bool.
  -- Therefor we can add an internal type that allows the type system to check this.
  HType :: HostType ty -> OhuaType ty s
  -- Tuple and Function types can capture both resolved an unresolved host types
  -- ToDo: Check if we need to introduce another layer of typing 
  TType :: NonEmpty (OhuaType ty s)                 -> OhuaType ty s
  FType :: FunType ty s                             -> OhuaType ty s
  IType :: InternalType ty Resolved                 -> OhuaType ty Resolved
  -- Every function abstr/appl. should have at least a unit argument
  -- To gurantee this after the typeSystem, while not adding those args in the typeSystem
  -- we need to have a UnitType accessible in Unresolved ... this is it:
  UType ::                                             OhuaType ty Unresolved
  TStar ::                                             OhuaType ty Unresolved

-- We need hashes for HM 
instance Hashable (OhuaType ty res) where
  hashWithSalt s oty = s

-- We need that for hashing
instance Eq (OhuaType ty res) where
  (==) = heq

deriving instance Show (OhuaType ty s)
instance Heq (OhuaType ty s1) (OhuaType ty s2) where
  heq (HType ty1) (HType ty2) = ty1 == ty2
  heq (IType ty1) (IType ty2) = heq ty1 ty2
  heq (FType fTy1)(FType fTy2) = heq fTy1 fTy2
  heq (TType tys1)(TType tys2) = length tys1 == length tys2 && all (uncurry heq) (NE.zip tys1 tys2)
  heq UType UType = True
  heq TStar TStar = True
  heq _ _ = False

type InternalType :: Type -> Resolution -> Type
data InternalType ty s where
  TypeNat :: InternalType ty s
  TypeBool :: InternalType ty s
  TypeUnit :: InternalType ty s
  TypeString :: InternalType ty s
  TypeList :: OhuaType ty s -> InternalType ty s
  -- TType :: NonEmpty (OhuaType ty s) -> InternalType ty s
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
  --heq (TType tys1) (TType tys2) =
    --NE.length tys1 == NE.length tys2 &&
    --all (uncurry heq) (NE.zip tys1 tys2)
  -- heq (TypeFunction ty1) (TypeFunction ty2) = heq ty1 ty2
  heq _ _ = False


type FunType :: Type -> Resolution -> Type
data FunType ty s where
     FunType :: Either () (NonEmpty (OhuaType ty s)) -> OhuaType ty s -> FunType ty s
     -- FIXME This is not properly defined.
     -- STFunType s [] t
     -- versus
     -- STFunType s [TypeUnit] t
     -- Yet formally, STFunType s [] t :: S -> T and that is ok.
     STFunType :: OhuaType ty s -> Either () (NonEmpty (OhuaType ty s)) -> OhuaType ty s -> FunType ty s

deriving instance Show (FunType ty s)

-- We need hashes for HM 
instance Hashable (FunType ty res) where
    hashWithSalt s ft = s

-- We need that for hashing
instance Eq (FunType ty res) where
  (==) = heq


instance Heq (FunType ty s1) (FunType ty s2) where
  heq (FunType args1 res1) (FunType args2 res2) =
    heqArgs args1 args2 &&
    heq res1 res2
  heq (STFunType s1 args1 res1) (STFunType s2 args2 res2) =
    heq s1 s2 &&
    heqArgs args1 args2 &&
    heq res1 res2
  heq _ _ = False


resToUnres :: OhuaType ty Resolved -> Maybe (OhuaType ty Unresolved)
-- HTpye gets it's resolution from Internal Type, which can only be resolved at this point so I can't pass it back
resToUnres (HType hty )   = Just $ HType hty 
resToUnres (TType tys) = case mapM resToUnres tys of
      Just rTys -> Just $ TType rTys
      Nothing -> Nothing
resToUnres (FType (FunType argsTys retTy))  = do
  fTy <- FunType  <$> mapArgs resToUnres argsTys <*> resToUnres retTy
  return (FType fTy)
resToUnres (FType (STFunType stTy argsTys retTy))  = do
  fTy <- STFunType <$> resToUnres stTy <*> mapArgs resToUnres argsTys <*> resToUnres retTy
  return (FType fTy)
resToUnres (IType _ )    = Nothing

unresToRes :: OhuaType ty Unresolved -> Maybe (OhuaType ty Resolved)
-- HTpye gets it's resolution from Internal Type, which can only be resolved at this point so I can't pass it back
unresToRes (HType hty)   = Just $ HType hty 
unresToRes (TType tys) = case mapM unresToRes tys of
      Just rTys -> Just $ TType rTys
      Nothing -> Nothing
unresToRes (FType (STFunType sin ins out)) = do
      sin' <- unresToRes sin
      ins' <- case ins of
        Left _ -> Just $ Left ()
        Right tys -> mapM unresToRes tys <&> Right
      out' <- unresToRes out
      return (FType (STFunType sin' ins' out'))
unresToRes (FType (FunType ins out)) = do
      ins' <- mapArgs unresToRes ins
      out' <- unresToRes out
      return (FType (FunType ins' out'))

unresToRes UType       = Just $ IType TypeUnit
unresToRes TStar       = Nothing


isUnresolved :: OhuaType ty Unresolved -> Bool
isUnresolved t = case t of
         (HType _ ) -> True
         (TType tys) -> any isUnresolved tys
         (FType (FunType (Left _) out)) -> isUnresolved out
         (FType (FunType (Right ins) out)) -> any isUnresolved (out NE.<| ins )
         (FType (STFunType state (Left _ ) out)) -> any isUnresolved (state :| [out])
         (FType (STFunType state (Right ins) out)) -> any isUnresolved (state NE.<| (out NE.<|  ins ))
         UType -> True
         TStar -> True


mapArgs ::Monad m => 
  (OhuaType ty sI -> m (OhuaType ty sO))
  -> Either () (NonEmpty (OhuaType ty sI)) 
  -> m (Either () (NonEmpty (OhuaType ty sO)))
mapArgs fun (Left _ )     = return $ Left ()
mapArgs fun (Right args)  = mapM fun args <&> Right

heqArgs ::
  Either () (NonEmpty (OhuaType ty s1))
  -> Either () (NonEmpty (OhuaType ty s2)) 
  -> Bool
heqArgs (Left _ )     (Left _ )      = True
heqArgs (Right args1) (Right args2)  = 
    length args1 == length args2 &&
    all (uncurry heq) (NE.zip args1 args2)
heqArgs a1            a2             = False 
--------------------------------------------------------------
--               Representation of Variables
--------------------------------------------------------------

-- | A typed Binding
data TypedBinding ty = TBind Binding (OhuaType ty Resolved) deriving (Generic)

asType:: TypedBinding ty -> OhuaType ty Resolved
asType (TBind _b ty) = ty

asBnd:: TypedBinding ty -> Binding
asBnd (TBind b _ty) = b

deriving instance Show (TypedBinding ty)

instance Hashable (TypedBinding ty) where
    hashWithSalt s (TBind b ty) = hashWithSalt s b

instance Ord (TypedBinding ty) where
    (TBind b1 ty1) <= (TBind b2 ty2) = b1 <= b2

-- FIXME: This is just a hack until we get everything typed correctly
-- currently 'reduceLambdas' in ALang Passes will loop forever if types dont match
-- As long as we can not make sure, that every binding and usage side is correctly typed, tranformations, in particular the ones that determine if something is used
-- should only check if something with the same name, not necesarily the same type annotation is used.
instance Eq (TypedBinding ty) where
    (TBind b1 ty1) == (TBind b2 ty2) = b1 == b2

--------------------------------------------------------------
--             Representation of Functions
--------------------------------------------------------------


-- This is not very elegant but saves me a ton of pattern matching in the TypeSystem
getReturnType :: OhuaType ty res -> Maybe (OhuaType ty res)
getReturnType (FType (FunType _ins out)) = Just out
getReturnType (FType (STFunType _s _ins out)) = Just out
getReturnType _ = Nothing


type FunRef :: Type -> Resolution -> Type
data FunRef ty s where
    FunRef :: QualifiedBinding -> FunType ty s -> FunRef ty s

instance Hashable (FunRef ty s) where 
  hashWithSalt s (FunRef qbnd _ ) = hashWithSalt s qbnd

getRefType :: FunRef ty s -> FunType ty s
getRefType (FunRef _q funTy) = funTy
--getRefReturnType (FunRef _q _i funTy) = getReturnType funTy

--------------------------------------------------------------
--                           Instances
--------------------------------------------------------------

deriving instance Show (FunRef ty s)
instance Eq (FunRef ty s) where
  (FunRef qb1 ty1) == (FunRef qb2 ty2) = qb1 == qb2 && heq ty1 ty2

-- Shortcut to generate controle node types
controlSignalType :: OhuaType ty Resolved
controlSignalType = TType $ IType TypeBool:| [IType TypeNat]