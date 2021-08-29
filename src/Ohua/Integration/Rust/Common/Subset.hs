{-# LANGUAGE DeriveLift, TemplateHaskell #-}
module Ohua.Integration.Rust.Common.Subset where

import Ohua.Prelude

import Language.Rust.Syntax (Ty)

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated


newtype RustType = RustType (Ty ()) deriving (Eq, Generic)

data Pat = IdentP IdentPat | TupP [IdentPat] deriving (Eq, Generic)
data IdentPat = IdentPat BindingMode Binding deriving (Eq, Generic)
data BindingMode = Mutable | Immutable deriving (Eq, Generic)

data Stmt e
  = Semi e
  | NoSemi e
  | Local Pat e
  deriving (Eq, Generic, Functor, Foldable, Traversable)

data Block e = RustBlock [Stmt e] Unsafety deriving (Eq, Generic, Functor, Foldable, Traversable)

data Unsafety = Normal deriving (Eq, Generic)

data BinOp = Add | Sub | Mul | Div deriving (Eq, Generic)
data UnOp = Not | Neg | Deref deriving (Eq, Generic)
data CallRef = CallRef QualifiedBinding (Maybe GenericArgs) deriving (Eq, Generic)

newtype GenericArgs = AngleBracketed [GenericArg] deriving (Eq, Generic)
newtype GenericArg = TypeArg RustType deriving (Eq, Generic)


-------------------- Recursion schemes support --------------------

makeBaseFunctor ''Pat
makeBaseFunctor ''Stmt
makeBaseFunctor ''Block

instance Plated Pat where plate = gplate
instance Plated (Stmt e)  where plate = gplate
instance Plated (Block e) where plate = gplate
