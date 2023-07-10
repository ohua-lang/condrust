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

module Ohua.Types.Reference where
{-
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

{-
data HostType ty where
    HostType::(Pretty ty, Show ty, Eq ty) =>  ty -> HostType ty

instance Show (HostType ty) where
    show (HostType ty) = show ty

instance Lift (HostType ty) where
  lift (HostType ty) = [|hosttype _ |]

instance Pretty (HostType ty) where
    pretty (HostType ty) = pretty ty

------------------------------------------------------------------------------------
--  Generics implementation for GADTS taken from the example described at 
--  https://ryanglscott.github.io/2018/02/11/how-to-derive-generic-for-some-gadts/
------------------------------------------------------------------------------------
data ECC :: Constraint -> (Type -> Type) -> Type -> Type where
  ECC :: c => { unECC :: f a } -> ECC c f a

instance (c => Show (f a)) => Show (ECC c f a) where
  show (ECC x) = show x

instance (c => Pretty (f a)) => Pretty (ECC c f a) where
  pretty (ECC x) = pretty x

instance (c, Eq (f x)) => Eq (ECC c f x) where
  ECC x == ECC y = x == y

instance Generic (HostType ty) where
  type Rep (HostType ty) = (ECC (Pretty ty, Show ty, Eq ty) (Rec0 ty))

  from (HostType x) = ECC (K1 x)
  to (ECC (K1 x)) = HostType x

-}


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

data UnresolvedVarType ty
    = UTypeNat
    | UTypeBool
    | UTypeUnit
    | UTypeString
    | UTypeList (UnresolvedVarType ty)
    | UType (HostType ty)
    | UTupleTy (NonEmpty (UnresolvedVarType ty))
    -- REMINDER: Can't derive Lift for Unit, therefor not for FunType and therefor I can't have FunType here for now
    --           Find a way to fix this
    | UTypeFunction (UnresolvedFunType ty) -- This is mainly used for inlined algos
    | TypeVar
    deriving (Lift, Generic)

data UnresolvedFunType ty where
     -- arguments types -> return type -> function type 
     UFunType :: [UnresolvedVarType ty] -> UnresolvedVarType ty -> UnresolvedFunType ty
     -- state/object type -> return type -> function type 
     USTFunType :: UnresolvedVarType ty -> [UnresolvedVarType ty] -> UnresolvedVarType ty -> UnresolvedFunType ty
     deriving (Lift)

instance EqNoType (UnresolvedVarType ty) where
    UTypeNat ~= UTypeNat = True
    UTypeBool ~= UTypeBool = True
    UTypeUnit ~= UTypeUnit = True
    UTypeString ~= UTypeString = True
    UType (HostType ty1) ~= UType (HostType ty2) = ty1 == ty2
    (UTupleTy ts) ~= (UTupleTy ts') = ts == ts' -- turns into ~=, see instance below
    (UTypeList inner1) ~= (UTypeList inner2) = inner1 == inner2
    (UTypeFunction fty1) ~= (UTypeFunction fty2) = fty1 == fty2
    _ ~= _ = False

instance Eq (UnresolvedVarType ty) where
    (==) = (~=)

instance ShowNoType (UnresolvedVarType ty) where
    showNoType UTypeNat = "INat"
    showNoType UTypeBool = "IBool"
    showNoType UTypeUnit = "IUnit"
    -- Is it internal though?
    showNoType UTypeString = "IString"
    showNoType (UTypeList ts) = "IList [" <> showNoType ts <> "]"
    showNoType (UType (HostType ty)) = show (pretty ty)
    showNoType (UTupleTy ts) = "(" <>  foldl (\b a -> show a <> ", " <> b) ")" ts
    showNoType (UTypeFunction fTy) = "Fun::" <> show fTy
    showNoType TypeVar = "TypeVar"


instance Show (UnresolvedVarType ty) where
    show = T.unpack . showNoType

instance Hashable (UnresolvedVarType ty) where
    hashWithSalt s _ = s

deriving instance Show (UnresolvedFunType ty)
deriving instance Eq (UnresolvedFunType ty)
deriving instance Generic (UnresolvedFunType ty)
instance Hashable (UnresolvedFunType ty)


data VarType ty
    = TypeNat
    | TypeBool
    | TypeUnit
    | TypeString
    | TypeList (VarType ty)
    | Type (HostType ty)
    | TupleTy (NonEmpty (VarType ty))
    -- REMINDER: Can't derive Lift for Unit, therefor not for FunType and therefor I can't have FunType here for now
    --           Find a way to fix this
    | TypeFunction (FunType ty) -- This is mainly used for inlined algos
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


instance Show (VarType ty) where
    show = T.unpack . showNoType

instance Hashable (VarType ty) where
    hashWithSalt s TypeNat = s
    hashWithSalt s TypeBool = s
    hashWithSalt s TypeUnit = s
    hashWithSalt s TypeString = s
    hashWithSalt s (TypeList ts) = s
    hashWithSalt s (Type _) = s
    hashWithSalt s (TupleTy _) = s
    hashWithSalt s (TypeFunction fty) = s

deriving instance Show (FunType ty)
deriving instance Eq (FunType ty)
deriving instance Generic (FunType ty)
instance Hashable (FunType ty)

--------------------------------------------------------------
--               Representation of Variables
--------------------------------------------------------------

-- | A binding name
newtype Binding =
    Binding Text
    deriving (Eq, Hashable, Generic, Ord, Monoid, Semigroup, NFData, Lift)

instance Show Binding where
    show (Binding name) = show name

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

{-
-- | Hierarchical reference to a namespace
newtype NSRef =
    NSRef [Binding]
    deriving (Eq, Generic, NFData, Ord, Lift)

instance Show NSRef where
    show (NSRef bnds) =show $  concatMap (("::" ++) . show) bnds 

-- | A qualified binding. References a particular bound value inside a
-- namespace.
declareFields [d|
    data QualifiedBinding = QualifiedBinding
        { qualifiedBindingNamespace :: NSRef
        , qualifiedBindingName      :: Binding
        } deriving (Eq, Generic, Ord, Lift)
  |]

instance Show QualifiedBinding where
    show (QualifiedBinding (NSRef scopeBnds) funBnd) = show $ foldr (\n o -> n <>"::"<> o) funBnd scopeBnds

-}
--------------------------------------------------------------
--             Representation of Functions
--------------------------------------------------------------


-- Actually we can only do it this way, i.e. without involving the argument type
-- at each call side because we assume that either generics are not allowed or
-- are also allowed in the backend such that we can consider a generic return type 
-- as fully resolved.
getReturnType :: UnresolvedFunType ty -> UnresolvedVarType ty
getReturnType (UFunType _ins out) = out
getReturnType (USTFunType _s _ins out) = out

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
    FunRef :: QualifiedBinding -> Maybe FnId -> UnresolvedFunType ty -> FunRef ty

getRefType (FunRef _q _i funTy) = funTy
getRefReturnType (FunRef _q _i funTy) = getReturnType funTy

--------------------------------------------------------------
--                           Instances
--------------------------------------------------------------

deriving instance Show (FunRef ty)
deriving instance Eq (FunRef ty)
deriving instance Generic (FunRef ty)
instance Hashable (FunRef ty)
-}
