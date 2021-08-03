{-# LANGUAGE DeriveLift, TemplateHaskell #-}
module Ohua.Integration.Rust.Backend.Subset where

import Ohua.Prelude hiding (Nat)
import Ohua.Types.Vector (Nat)
import Ohua.Integration.Rust.TypeExtraction
import Language.Rust.Parser (Span)

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated


data Expr
  = Lit (Lit (RustArgType Span))
  | Var Binding
  | MethodCall Expr Binding [Expr]
  | Call QualifiedBinding [Expr]
  | Binary BinOp Expr Expr
  | Unary UnOp Expr
  | Local Pat Expr
  | Assign Expr Expr
  | Loop Block
  | ForLoop IdentPat Range Block
  | While Expr Block
  | If Expr Block (Maybe Block)
  | Tuple [Expr]
  | TupleField Expr Nat
  | Try Expr
  | BlockExpr Block
  deriving (Eq, Generic)

data Stmt
  = Semi Expr
  | NoSemi Expr
  deriving (Eq, Generic)

newtype Block = Block [Stmt] deriving (Eq, Generic)

data BinOp = Add | Sub | Mul | Div deriving (Eq, Generic)
data UnOp = Not | Neg | Deref deriving (Eq, Generic)
data Pat = IdentP IdentPat | TupP IdentPat IdentPat deriving (Eq, Generic)
data IdentPat = IdentPat BindingMode Binding RustType deriving (Eq, Generic)
data BindingMode = Mutable | Immutable deriving (Eq, Generic)
data Range = Range Nat (Maybe Nat) deriving (Eq, Generic)

-- TODO this should really be much more expressive and allow for RustArgTypes
data RustType = TypeHole | TupleTyp RustType RustType deriving (Eq, Generic)


-------------------- Recursion schemes support --------------------

makeBaseFunctor ''Stmt
makeBaseFunctor ''Expr
makeBaseFunctor ''Block
makeBaseFunctor ''Pat

instance Plated Stmt where plate = gplate
instance Plated Expr where plate = gplate
instance Plated Block where plate = gplate
instance Plated Pat where plate = gplate
