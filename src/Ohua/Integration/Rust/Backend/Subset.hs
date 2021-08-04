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
  | Assign Expr Expr
  | Loop Block
  | ForLoop IdentPat Range Block
  | While Expr Block
  | If Expr Block (Maybe Expr)
  | Tuple [Expr]
  | TupleField Expr Nat
  | Try Expr
  | BlockExpr Block
  deriving (Eq, Generic)

data Stmt
  = Semi Expr
  | NoSemi Expr
  | Local Pat Expr
  deriving (Eq, Generic)

newtype Block = Block [Stmt] deriving (Eq, Generic)

data BinOp = Add | Sub | Mul | Div deriving (Eq, Generic)
data UnOp = Not | Neg | Deref deriving (Eq, Generic)
data Pat = IdentP IdentPat | TupP [IdentPat] deriving (Eq, Generic)
data IdentPat = IdentPat BindingMode Binding deriving (Eq, Generic)
data BindingMode = Mutable | Immutable deriving (Eq, Generic)
data Range = Range (Maybe Expr) (Maybe Expr) deriving (Eq, Generic)

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

--transformExpr = transform

transformExprInBlock :: (Expr -> Expr) -> Block -> Block
transformExprInBlock f = goBlock
  where
    go (BlockExpr block) = BlockExpr $ goBlock block
    go (If e0 block e1) = If e0 (goBlock block) e1
    go (Loop block) = Loop $ goBlock block
    go (ForLoop p r block) = ForLoop p r $ goBlock block
    go (While e block) = While e $ goBlock block
    go e = e

    goBlock (Block stmts) =
      let stmts' = reverse $ map goStmt (reverse stmts)
      in Block stmts'

    goStmt (Local p e) = Local p $ transform (f . go) e
    goStmt (Semi e) = Semi $ transform (f . go) e
    goStmt (NoSemi e) = NoSemi $ transform (f . go) e

