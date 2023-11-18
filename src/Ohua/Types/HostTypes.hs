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

module Ohua.Types.HostTypes where

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
import qualified Data.List.NonEmpty as NE

import System.FilePath.Posix (addExtension)
import System.FilePath as Path (joinPath)


import Ohua.Util
import Ohua.LensClasses
import Ohua.Types.Error
import Ohua.Types.Make
import Ohua.Types.Classes
import Ohua.Types.Unit (Unit)
import Ohua.Types.Bindings


import qualified Text.Show


-- | A wrapper type for the types of supported host languages allowing us to specify requried properties (constraints) 
--   host types must implement to process them properly.
data HostType ty where
  HostType::(Pretty ty, Show ty, Eq ty, Pathable ty, TruthableType ty, UnTupleType ty, ListElementType ty) => ty -> HostType ty

deriving instance Eq (HostType ty)


-- | Allows us to extract eventual namesspaces from host types e.g. toPath std::sync::Arc -> Just (Right [std, sync] Arc) 
class Pathable t where
  toPath :: t -> Maybe (Either Binding QualifiedBinding)

-- | We need to destruct and typecheck tuples during compilation. Tuples will have a tuple type in the host type representation. 
--   This allows us to get the single types contained in those tuples irrespective of the current host type.
--   We get a NonEmpty because obviously each hosttype contains at least one host type
class UnTupleType ty where
  unTupleType :: ty -> NonEmpty ty

-- | We want to support branching with conditions other than plain boolean literals.
--   To still be able to type check that for instance f(8) will return a boolean or something interpretable 
--   as a boolean in the host language, the HostType has to implement a function by which we can query whether
--   a type is (equivalent to) a boolean
class TruthableType ty where
  canbeBool :: ty -> Bool  

-- | To type(check) map expressions, we (currently) need to know which element type 
--   iterated objects return and need to be able to check that they return a "list like"
--   iterable structure we can interpret as an internal list type.
class ListElementType ty where
  asListElementType :: ty -> Maybe ty

instance Show (HostType ty) where
  show (HostType ty) = show ty

instance Lift (HostType ty) where
  lift (HostType ty) = [|hosttype _ |]

instance Pretty (HostType ty) where
  pretty (HostType ty) = pretty ty

instance Pathable (HostType ty) where
  toPath (HostType t) = toPath t

instance UnTupleType (HostType ty) where
  unTupleType (HostType ty) = NE.map HostType (unTupleType ty)

instance TruthableType (HostType ty) where
  canbeBool (HostType ty) = canbeBool ty

instance ListElementType (HostType ty) where
  asListElementType (HostType ty) = asListElementType ty <&> HostType
 
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
  type Rep (HostType ty) = (ECC (Pretty ty, Show ty, Eq ty, Pathable ty, UnTupleType ty, TruthableType ty, ListElementType ty) (Rec0 ty))

  from (HostType x) = ECC (K1 x)
  to (ECC (K1 x)) = HostType x

