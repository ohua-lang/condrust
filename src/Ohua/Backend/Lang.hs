{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Ohua.Backend.Lang where

import Ohua.Prelude

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)


data App expr
  = Stateless QualifiedBinding [expr]
  | Stateful Binding QualifiedBinding [expr]
  deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

data Recv 
  = Recv
      Int -- copy index 
      Binding -- channel

data Send
  = Emit 
      Binding -- channel
      Binding -- data

data Channel = Channel 
                Binding -- channel id
                Int -- num of copies
                deriving (Show, Eq, Lift, Generic)

data List expr = Create | Append Binding expr 
  deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

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
  | First (Either Binding Lit)
  | Second (Either Binding Lit)

  | Increment Binding -- a + 1;
  | Decrement Binding -- a - 1;
  | Not TaskExpr
  | Generate Binding Lit -- data generator  
  
  deriving (Show, Eq, Lift, Generic)

data Function expr = Function QualifiedBinding [Binding] expr deriving (Show, Eq)

-------------------- Recursion schemes support --------------------

makeBaseFunctor ''App
makeBaseFunctor ''TaskExpr

