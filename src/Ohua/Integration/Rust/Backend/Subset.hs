--{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Ohua.Integration.Rust.Backend.Subset
  ( -- https://ro-che.info/articles/2019-01-26-haskell-module-system-p2
    -- export the constructors
    module Ohua.Integration.Rust.Common.Subset,
    module Ohua.Integration.Rust.Backend.Subset,
  )
where

import Control.Lens.Plated
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Language.Rust.Parser (Span)
import Language.Rust.Syntax (GenericArgs)
import Ohua.Integration.Rust.Common.Subset hiding (Block, Stmt)
import qualified Ohua.Integration.Rust.Common.Subset as CSTypes (Block, Stmt)
import Ohua.Integration.Rust.Types.Extraction
import Ohua.Prelude hiding (Nat)
import Ohua.Types.Vector (Nat)

type Block = CSTypes.Block Expr

type Stmt = CSTypes.Stmt Expr

data Expr
  = Lit (Lit RustVarType)
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
  deriving (Eq, Generic, Show)

-------------------- Recursion schemes support --------------------

makeBaseFunctor ''Expr

instance Plated Expr where plate = gplate

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

    goBlock (RustBlock unsafety stmts) = do
      stmts' <- reverse <$> mapM goStmt (reverse stmts)
      return $ RustBlock unsafety stmts'

    goStmt (Local p ty e) = Local p ty <$> transformM (f <=< go) e
    goStmt (Semi e) = Semi <$> transformM (f <=< go) e
    goStmt (NoSemi e) = NoSemi <$> transformM (f <=< go) e
    goStmt StandaloneSemi = return StandaloneSemi
