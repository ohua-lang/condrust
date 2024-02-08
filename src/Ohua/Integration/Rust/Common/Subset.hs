{-# LANGUAGE DeriveLift, TemplateHaskell #-}
module Ohua.Integration.Rust.Common.Subset where

import Ohua.Prelude
-- import Ohua.Types
import Ohua.Integration.Rust.Util (toBinding)

-- import Language.Rust.Data.Ident (Ident(..))
import Language.Rust.Syntax (Ty(..), Path(..), PathSegment(..))


import Data.Functor.Foldable.TH (makeBaseFunctor)
-- import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated


newtype RustType = RustType (Ty ()) deriving (Show, Eq, Generic)

data Pat = IdentP IdentPat | TupP [IdentPat] | WildP deriving (Show, Eq, Generic)
data IdentPat = IdentPat BindingMode Binding deriving (Show, Eq, Generic)
data BindingMode = Mutable | Immutable deriving (Show, Eq, Generic)

data Stmt e
  = Semi e
  | NoSemi e
  | Local Pat (Maybe RustType) e
  | StandaloneSemi
  deriving (Eq, Generic, Functor, Foldable, Traversable, Show)

data Block e = RustBlock Unsafety [Stmt e]
  deriving (Eq, Generic, Functor, Foldable, Traversable, Show)

data Unsafety = Normal deriving (Eq, Generic, Show)

data BinOp = Add | Sub | Mul | Div | Lt | Lte | Gt | Gte | EqOp | OrOp  deriving (Show, Eq, Generic)
data UnOp = Not | Neg | Deref deriving (Show, Eq, Generic)
data CallRef = CallRef QualifiedBinding (Maybe GenericArgs) deriving (Show, Eq, Generic)
data TyRef = TyRef QualifiedBinding (Maybe GenericArgs) deriving (Show, Eq, Generic)

data GenericArgs
  = AngleBracketed [GenericArg]
  | Parenthesized [TyRef] (Maybe TyRef) deriving (Show, Eq, Generic)
newtype GenericArg = TypeArg RustType deriving (Show, Eq, Generic)

-------------------- Recursion schemes support --------------------

makeBaseFunctor ''Pat
makeBaseFunctor ''Stmt
makeBaseFunctor ''Block

instance Plated Pat where plate = gplate
instance Plated (Stmt e)  where plate = gplate
instance Plated (Block e) where plate = gplate

-------------------- Instances --------------------

instance Pathable RustType where
  toPath =
    \case
          (RustType (PathTy Nothing (Path _ (p:ps) _) _)) ->
            let (p':|ps') = map convertSegment (p:|ps)
            in case ps' of
                [] -> Just $ Left p'
                _ -> Just $ Right $ QualifiedBinding (NSRef ps') p'
          _any -> Nothing
    where
      convertSegment (PathSegment ident _ _) = toBinding ident
