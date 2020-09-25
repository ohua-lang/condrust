{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Ohua.Backend.Lang where

import Ohua.Prelude

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated


data App expr
  = Stateless QualifiedBinding [expr]
  | Stateful Binding QualifiedBinding [expr]
  deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

data Recv 
  = Recv
      Int -- copy index 
      Binding -- channel
  deriving (Eq, Show, Generic)

instance Hashable Recv

data Send
  = Emit 
      Binding -- channel
      Binding -- data
  deriving (Eq, Show)

data Channel = Channel 
                Binding -- channel id
                Int -- num of copies
                deriving (Show, Eq, Lift, Generic)

data List expr = Create | Append Binding expr 
  deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

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
-- FIXME use the types from above!
  | Receive Int -- copy index 
            Binding -- channel
  | Send Binding -- channel
         Binding -- data

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

data Function expr = Function QualifiedBinding [Binding] expr deriving (Show, Eq)

-------------------- Recursion schemes support --------------------

makeBaseFunctor ''App
makeBaseFunctor ''TaskExpr

instance Plated TaskExpr where plate = gplate
