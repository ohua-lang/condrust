{-# LANGUAGE DeriveLift, TemplateHaskell #-}
module Ohua.Integration.Rust.Frontend.Subset(
  module Ohua.Integration.Rust.Frontend.Subset,
  module Ohua.Integration.Rust.Common.Subset
                                            ) where

import Ohua.Prelude hiding (Lit)
import Ohua.Integration.Rust.Common.Subset hiding (Block, Stmt)
import qualified Ohua.Integration.Rust.Common.Subset as CS (Block(..), Stmt(..))

import Language.Rust.Syntax (Ty)

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated

type Block = CS.Block Expr
type Stmt = CS.Stmt Expr


-- | This is the supported subset of the Rust language.
data Expr
  = Call Expr [Expr]
  | MethodCall Expr CallRef [Expr]
  | Tuple [Expr]
  | Binary BinOp Expr Expr
  | Unary UnOp Expr
--  | Lit (Lit (RustArgType Span))
  | If Expr Block (Maybe Expr)
  | While Expr Block
  | ForLoop Pat Expr Block
  | Closure CaptureBy IsAsync Movability [Arg] (Maybe RustType) Expr
  | BlockExpr Block
  -- | the below two are just captured by a Path in Rust.
  | PathExpr CallRef
  | Var VarRef
  | Lit Lit
  deriving (Eq, Generic)


data Lit = Int Integer | Bool Bool deriving (Eq, Generic)
type VarRef = Binding
data Arg = Arg Pat RustType deriving (Eq, Generic)

data CaptureBy = Value deriving (Eq, Generic)
data IsAsync = NotAsync deriving (Eq, Generic)
data Movability = Movable deriving (Eq, Generic)

-- data PathExpr ty = Path [PathSegment] (Maybe ty) deriving (Eq, Generic)
-- newtype PathSegment = PathSegment Ident deriving (Eq, Generic)
-- newtype Ident = Text deriving (Eq, Generic)

-------------------- Recursion schemes support --------------------

makeBaseFunctor ''Expr

instance Plated Expr   where plate = gplate
