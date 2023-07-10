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


import qualified Text.Show


data HostType ty where
    HostType::(Pretty ty, Show ty, Eq ty) =>  ty -> HostType ty

deriving instance Eq (HostType ty)

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

