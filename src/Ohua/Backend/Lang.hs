{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Ohua.Backend.Lang where

import Ohua.Prelude hiding (First)

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated


data App expr
  = Stateless QualifiedBinding [expr]
  | Stateful expr QualifiedBinding [expr]
  deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

instance (Hashable expr) => Hashable (App expr)

data ComType = Channel | Recv | Send deriving (Show, Eq, Generic)

data Com (f::ComType) (t::Type) :: Type where
  SChan :: Binding -> Com 'Channel t
  SRecv :: ArgType t -> Com 'Channel t -> Com 'Recv t
  SSend :: Com 'Channel t -> Binding -> Com 'Send t

deriving instance Eq (Com semTy ty)

type Channel = Com 'Channel

instance Hashable (Com a t) where
  hashWithSalt s (SChan bnd) = s `hashWithSalt` bnd
  hashWithSalt s (SRecv t chan) = s `hashWithSalt` t `hashWithSalt` chan
  hashWithSalt s (SSend chan bnd) = s `hashWithSalt` chan `hashWithSalt` bnd

data List expr = Create | Append Binding expr 
  deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

instance (Hashable expr) => Hashable (List expr)

-- TODO this expression language should be typed, probably using a GADT
data TaskExpr ty
  = Var Binding
  | Lit (Lit ty) -- true, false  etc.
  | Apply (App (TaskExpr ty))
  | Let Binding
        (TaskExpr ty)
        (TaskExpr ty) -- cont
  | Stmt 
      (TaskExpr ty)
      (TaskExpr ty) -- cont
  | Assign -- side-effect
      Binding
      (TaskExpr ty)
  | ReceiveData (Com 'Recv ty)
  | SendData (Com 'Send ty)

  -- specific control flow:
  | EndlessLoop (TaskExpr ty)
  | ForEach Binding Binding (TaskExpr ty) -- ^ a.k.a. map
  | Repeat (Either Binding Int) (TaskExpr ty)
  | While (TaskExpr ty) (TaskExpr ty)
  | Cond (TaskExpr ty) (TaskExpr ty) (TaskExpr ty)
  
  -- specific functions:
  | HasSize Binding -- :: [a] -> Bool
  | Size Binding -- :: [a] -> Int

  | ListOp (List (TaskExpr ty))

  | Tuple (Either Binding (Lit ty)) (Either Binding (Lit ty))
  | First Binding
  | Second Binding

  | Increment Binding -- a + 1;
  | Decrement Binding -- a - 1;
  | Not (TaskExpr ty)
  | Generate Binding (Lit ty) -- data generator  
  
  deriving (Eq,Generic)

instance Hashable (TaskExpr ty)

data Function expr = Function QualifiedBinding [Binding] expr deriving (Show, Eq)

-------------------- Recursion schemes support --------------------

makeBaseFunctor ''App
makeBaseFunctor ''TaskExpr

instance Plated (TaskExpr ty) where plate = gplate
