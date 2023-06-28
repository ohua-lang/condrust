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
#-}

module Ohua.Types.Unresolved.Reference where

import Universum hiding (Nat, toList)

import GHC.Generics
import Control.Lens.TH
import Control.Lens.Plated
import Instances.TH.Lift ()
import Data.Text.Prettyprint.Doc (Pretty(..))
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

data VarType ty
    = TypeNat
    | TypeBool
    | TypeUnit
    | TypeString
    | TypeList (VarType ty)
    | Type (HostType ty)
    | TupleTy (NonEmpty (VarType ty))
    | TypeFunction (FunType ty)
    | TypeVar
    deriving (Lift, Generic)

data FunType ty where
     -- arguments types -> return type -> function type 
     FunType :: [VarType ty] -> VarType ty -> FunType ty
     -- state/object type -> return type -> function type 
     STFunType :: VarType ty -> [VarType ty] -> VarType ty -> FunType ty
     deriving (Lift)

-- ToDo: This is just a helper until we get types of control nodes right
controlSignalType :: VarType ty
controlSignalType = TupleTy $ TypeBool:| [TypeNat]


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


--------------------------------------------------------------
--               Representation of Variables
--------------------------------------------------------------

-- | A typed Binding
data TypedBinding ty = TBind Binding (VarType ty) deriving (Show, Generic)

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

asBnd :: TypedBinding ty -> Binding
asBnd (TBind bnd _ty) = bnd

asType :: TypedBinding ty -> VarType ty
asType (TBind _bnd ty) = ty

--------------------------------------------------------------
--             Representation of Functions
--------------------------------------------------------------


-- Actually we can only do it this way, i.e. without involving the argument type
-- at each call side because we assume that either generics are not allowed or
-- are also allowed in the backend such that we can consider a generic return type 
-- as fully resolved.
getReturnType :: FunType ty -> VarType ty
getReturnType (FunType _ins out) = out
getReturnType (STFunType _s _ins out) = out

pureArgTypes :: FunType ty -> [VarType ty]
pureArgTypes (FunType ins _out) = ins
pureArgTypes (STFunType s ins _out) = ins

stateArgTypes :: FunType ty -> Maybe (VarType ty)
stateArgTypes (FunType _ins _out) = Nothing
stateArgTypes (STFunType s _ins _out) = Just s

setReturnType :: VarType ty -> FunType ty -> FunType ty
setReturnType ty (FunType ins out) = FunType ins ty
setReturnType ty (STFunType s ins out) = STFunType s ins ty

setFunType :: [VarType ty] -> VarType ty -> FunType ty -> FunType ty
setFunType intys outty (FunType _i _out) = FunType intys outty
setFunType intys outty (STFunType s _ins _out) = STFunType s intys outty 


data FunRef ty where
    FunRef :: QualifiedBinding -> Maybe FnId -> FunType ty -> FunRef ty

getRefType (FunRef _q _i funTy) = funTy
getRefReturnType (FunRef _q _i funTy) = getReturnType funTy

--------------------------------------------------------------
--                           Instances
--------------------------------------------------------------

deriving instance Show (FunRef ty)
deriving instance Eq (FunRef ty)
deriving instance Generic (FunRef ty)
instance Hashable (FunRef ty)
