{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE QuantifiedConstraints #-}
module Ohua.Backend.Lang where

import Ohua.Prelude

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated


data App expr
  = Stateless QualifiedBinding [expr]
  | Stateful expr QualifiedBinding [expr]
  deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

instance (Hashable expr) => Hashable (App expr)

data ComType = Channel | Recv | Send deriving (Show, Eq, Generic)

data Com :: ComType -> Type where
  SChan :: Binding -> Com 'Channel
  SRecv :: Com 'Channel -> Com 'Recv
  SSend :: Com 'Channel -> Binding -> Com 'Send

type Channel = Com 'Channel

deriving instance Show (Com a)
deriving instance Eq (Com a)
deriving instance Lift (Com a)

instance Hashable (Com a) where
  hashWithSalt s (SChan bnd) = s `hashWithSalt` bnd
  hashWithSalt s (SRecv chan) = s `hashWithSalt` chan
  hashWithSalt s (SSend chan bnd) = s `hashWithSalt` chan `hashWithSalt` bnd

data List expr = Create | Append Binding expr 
  deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

instance (Hashable expr) => Hashable (List expr)

-- TODO this expression language should be typed, probably using a GADT
data TaskExpr
  = Var Binding
  | Lit Lit -- true, false  etc.
  | Apply (App TaskExpr)
  | Let Binding
        TaskExpr
        TaskExpr -- cont
  | Stmt 
      TaskExpr
      TaskExpr -- cont
  | Assign -- side-effect
      Binding
      TaskExpr
  | ReceiveData (Com 'Recv)
  | SendData (Com 'Send)

  -- specific control flow:
  | EndlessLoop TaskExpr
  | ForEach Binding Binding TaskExpr -- ^ a.k.a. map
  | Repeat (Either Binding Int) TaskExpr
  | While TaskExpr TaskExpr
  | Cond TaskExpr TaskExpr TaskExpr
  
  -- specific functions:
  | HasSize Binding -- :: [a] -> Bool
  | Size Binding -- :: [a] -> Int

  | ListOp (List TaskExpr)

  | Tuple (Either Binding Lit) (Either Binding Lit)
  | First Binding
  | Second Binding

  | Increment Binding -- a + 1;
  | Decrement Binding -- a - 1;
  | Not TaskExpr
  | Generate Binding Lit -- data generator  
  
  deriving (Show, Eq, Lift, Generic)

instance Hashable TaskExpr

data Function expr = Function QualifiedBinding [Binding] expr deriving (Show, Eq)

-------------------- Recursion schemes support --------------------

makeBaseFunctor ''App
makeBaseFunctor ''TaskExpr

instance Plated TaskExpr where plate = gplate
