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

import qualified Data.List.NonEmpty as NE (length, zip)

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
-- import Ohua.Types.Vector (Nat(..))
import Ohua.Types.Bindings
import Ohua.Types.HostTypes

import qualified Text.Show


-- | Internal type representations. While Type and TupleTy capture types from the
--   host language, the literal types TypeNat and TypeBool etc. are used internaly to construct nodes
--   They must be mapped to the according types of the host language in the backend.


{-
I'm really tired of dependent types in Haskell.
Let's just move on and use Coq already!

data Resolution = Resolved | Unresolved deriving (Eq, Show, Lift)

type family ResMin (l::Resolution) (r::Resolution) :: Resolution where
  ResMin 'Resolved   'Resolved   = 'Resolved
  ResMin 'Unresolved _           = 'Unresolved
  ResMin _           'Unresolved = 'Unresolved

data RList f ty :: Nat -> Resolution -> * where
  RNil  ::                               RList f ty  'Zero    'Resolved
  RCons :: f ty r1 -> RList f ty s r2 -> RList f ty ('Succ s) (ResMin r1 r2)

instance IsList (RList f ty s r) where
  type Item (RList f ty s r) = f ty r

  --fromList :: [f ty] -> RList f ty s r
  fromList l =
    case l of
      [] -> RNil
      (x:xs) -> case fromList xs of
                  RNil -> RCons x RNil :: RList f ty ('Succ 'Zero) 'Resolved
                  xs' -> RCons x xs'

  toList RNil = []
  toList (RCons x xs) = x : toList xs


data FType ty :: Resolution -> * where
   -- arguments types -> return type -> function type
   FunType   ::                RList VType ty s r1 -> VType ty r2 -> FType ty (ResMin r1 r2)
   -- state/object type -> return type -> function type
   STFunType :: VType ty r1 -> RList VType ty s r2 -> VType ty r3 -> FType ty (ResMin r1 (ResMin r2 r3))

data VType ty :: Resolution -> * where
  TypeVar      :: VType ty 'Unresolved
  TypeNat      :: VType ty 'Resolved
  TypeBool     :: VType ty 'Resolved
  TypeUnit     :: VType ty 'Resolved
  TypeString   :: VType ty 'Resolved
  TypeHost     :: HostType ty -> VType ty 'Resolved
  TypeList     :: VType ty r -> VType ty r
  TypeTuple    :: RList VType ty ('Succ n) r -> VType ty r
  TypeFunction :: FType ty r -> VType ty r

type VarType ty = VType ty 'Resolved
type FunType ty = FType ty 'Resolved
-}

-- Because dependent types in Haskell suck, I have to replicate the data structure

-- | Heterogeneous equality taken from here:
-- https://github.com/sweirich/dth/blob/master/nested-datatypes/Nested.lhs
class Heq a b where
  heq :: a -> b -> Bool

data Resolution = Unresolved | Resolved

type OhuaType :: Type -> Resolution -> Type
data OhuaType ty s where
  HType :: HostType ty -> Maybe (InternalType ty s) -> OhuaType ty s
  IType :: InternalType ty Resolved                 -> OhuaType ty Resolved
  TStar ::                                             OhuaType ty Unresolved

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
  TupleTy :: NonEmpty (OhuaType ty s) -> InternalType ty s
  TypeFunction :: FunType ty s -> InternalType ty s

deriving instance Show (InternalType ty s)
instance Heq (InternalType ty s1) (InternalType ty s2) where
  heq TypeNat TypeNat = True
  heq TypeBool TypeBool = True
  heq TypeUnit TypeUnit = True
  heq TypeString TypeString = True
  heq (TypeList ty1) (TypeList ty2) = heq ty1 ty2
  heq (TupleTy tys1) (TupleTy tys2) =
    NE.length tys1 == NE.length tys2 &&
    (and $ map (uncurry heq) $ NE.zip tys1 tys2)
  heq (TypeFunction ty1) (TypeFunction ty2) = heq ty1 ty2
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
instance Heq (FunType ty s1) (FunType ty s2) where
  heq (FunType args1 res1) (FunType args2 res2) =
    NE.length args1 == NE.length args2 &&
    (and $ map (uncurry heq) $ NE.zip args1 args2) &&
    heq res1 res2
  heq (STFunType s1 args1 res1) (STFunType s2 args2 res2) =
    heq s1 s2 &&
    length args1 == length args2 &&
    (and $ map (uncurry heq) $ zip args1 args2) &&
    heq res1 res2
  heq _ _ = False

{-
data VarType ty
    = TypeNat (HostType ty)
    | TypeBool (HostType ty)
    | TypeUnit (HostType ty)
    | TypeString (HostType ty)
    | TypeList (VarType ty) (HostType ty
    | Type (HostType ty)
    | TupleTy (NonEmpty (VarType ty)) (HostType ty)
    | TypeFunction (FunType ty) (HostType ty)
    | TypeVar
    deriving (Lift, Generic)

data FunType ty where
     -- arguments types -> return type -> function type
     FunType :: NonEmpty (VarType ty) -> VarType ty -> FunType ty
     -- state/object type -> return type -> function type
     -- FIXME This is not properly defined.
     -- STFunType s [] t
     -- versus
     -- STFunType s [TypeUnit] t
     -- Yet formally, STFunType s [] t :: S -> T and that is ok.
     STFunType :: VarType ty -> [VarType ty] -> VarType ty -> FunType ty
     deriving (Lift)

-- ToDo: This is just a helper until we get types of control nodes right
controlSignalType :: VarType ty
controlSignalType = TupleTy $ TypeBool :| [TypeNat]
-}

{-
instance EqNoType (VarType ty) where
    TypeNat ~= TypeNat = True
    TypeBool ~= TypeBool = True
    TypeUnit ~= TypeUnit = True
    TypeString ~= TypeString = True
    Type (HostType ty1) ~= Type (HostType ty2) = ty1 == ty2
    (TupleTy ts) ~= (TupleTy ts') = ts == ts' -- turns into ~=, see instance below
    (TypeList inner1) ~= (TypeList inner2) = inner1 == inner2
    (TypeFunction fty1) ~= (TypeFunction fty2) = fty1 == fty2
    _ ~= _ = False

instance Eq (VarType ty) where
    (==) = (~=)

instance ShowNoType (VarType ty) where
    showNoType TypeNat = "INat"
    showNoType TypeBool = "IBool"
    showNoType TypeUnit = "IUnit"
    -- Is it internal though?
    showNoType TypeString = "IString"
    showNoType (TypeList ts) = "IList [" <> showNoType ts <> "]"
    showNoType (Type (HostType ty)) = show (pretty ty)
    showNoType (TupleTy ts) = "(" <>  foldl (\b a -> show a <> ", " <> b) ")" ts
    showNoType (TypeFunction fTy) = "Fun::" <> show fTy
    showNoType TypeVar = "TypeVar"


instance Show (VarType ty) where
    show = T.unpack . showNoType

instance Hashable (VarType ty) where
    hashWithSalt s _ = s

deriving instance Show (FunType ty)
deriving instance Eq (FunType ty)
deriving instance Generic (FunType ty)
instance Hashable (FunType ty)
-}

--------------------------------------------------------------
--               Representation of Variables
--------------------------------------------------------------

-- | A typed Binding
data TypedBinding ty = TBind Binding (OhuaType ty Resolved) deriving (Generic)

deriving instance Show (TypedBinding ty)

instance Hashable (TypedBinding ty) where
    hashWithSalt s (TBind b ty) = hashWithSalt s b

instance Ord (TypedBinding ty) where
    (TBind b1 _ty1) <= (TBind b2 _ty2) = b1 <= b2

-- FIXME: This is just a hack until we get everything typed correctly
-- currently 'reduceLambdas' in ALang Passes will loop forever if types dont match
-- As long as we can not make sure, that every binding and usage side is correctly typed, tranformations, in particular the ones that determine if something is used
-- should only check if something with the same name, not necesarily the same type annotation is used.
instance Eq (TypedBinding ty) where
    (TBind b1 _ty1) == (TBind b2 _ty2) = b1 == b2

{-
asBnd :: TypedBinding ty -> Binding
asBnd (TBind bnd _ty) = bnd

asType :: TypedBinding ty -> VarType ty
asType (TBind _bnd ty) = ty
-}

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
  (FunRef qb1 _ ty1) == (FunRef qb2 _ ty2) = qb1 == qb2 -- && heq ty1 ty2
-- deriving instance Generic (FunRef ty s)
-- instance Hashable (FunRef ty s)
