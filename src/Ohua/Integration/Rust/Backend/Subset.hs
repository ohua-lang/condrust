{-# LANGUAGE DeriveLift, TemplateHaskell #-}
module Ohua.Integration.Rust.Backend.Subset where

import Ohua.Prelude hiding (Nat)
import Ohua.Types.Vector (Nat)
import Ohua.Integration.Rust.TypeExtraction
import Language.Rust.Parser (Span)
import Language.Rust.Syntax (GenericArgs)

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated


data Expr
  = Lit (Lit (RustArgType Span))
  | Var Binding
  | MethodCall Expr CallRef [Expr]
  | Call CallRef [Expr]
  | Binary BinOp Expr Expr
  | Unary UnOp Expr
  | Assign Expr Expr
  | Loop Block
  | ForLoop IdentPat Expr Block
  | While Expr Block
  | If Expr Block (Maybe Expr)
  | Tuple [Expr]
  | TupleField Expr Nat
  | Try Expr
  | BlockExpr Block
  | HalfOpenRange (Maybe Expr) (Maybe Expr)
  | Async Block
  deriving (Eq, Generic)

data Stmt
  = Semi Expr
  | NoSemi Expr
  | Local Pat Expr
  deriving (Eq, Generic)

newtype Block = Block [Stmt] deriving (Eq, Generic)

data BinOp = Add | Sub | Mul | Div | Lt | Lte | Gt | Gte | EqOp | Neq deriving (Eq, Generic)
data UnOp = Not | Neg | Deref deriving (Eq, Generic)
data Pat = IdentP IdentPat | TupP [IdentPat] deriving (Eq, Generic)
data IdentPat = IdentPat BindingMode Binding deriving (Eq, Generic)
data BindingMode = Mutable | Immutable deriving (Eq, Generic)
data CallRef = CallRef QualifiedBinding (Maybe (GenericArgs ())) deriving (Eq, Generic)
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
transformExprInBlock f = runIdentity . transformExprInBlockM (pure . f)

transformExprInBlockM :: (Monad m) => (Expr -> m Expr) -> Block -> m Block
transformExprInBlockM f = goBlock
  where
    go (BlockExpr block) = BlockExpr <$> goBlock block
    go (If e0 block e1) = (\block' -> If e0 block' e1) <$> goBlock block
    go (Loop block) = Loop <$> goBlock block
    go (ForLoop p r block) = ForLoop p r <$> goBlock block
    go (While e block) = While e <$> goBlock block
    go e = pure e

    goBlock (Block stmts) = do
      stmts' <- reverse <$> mapM goStmt (reverse stmts)
      return $ Block stmts'

    goStmt (Local p e) = Local p <$> transformM (f <=< go) e
    goStmt (Semi e) = Semi <$> transformM (f <=< go) e
    goStmt (NoSemi e) = NoSemi <$> transformM (f <=< go) e

