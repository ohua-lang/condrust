{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Ohua.Backend.Lang where

import Ohua.Prelude

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)


newtype Task expr = Task expr deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

data App expr
  = Stateless QualifiedBinding [expr]
  | Stateful Binding QualifiedBinding [expr]
  deriving (Show, Eq, Lift, Generic, Functor, Foldable, Traversable)

data TaskExpr
  -- common with TCExpr
  = VarT Binding
  | ApplyT (App TaskExpr)
  | LetT Binding
        TaskExpr
        TaskExpr -- cont
  | Stmt 
      TaskExpr
      TaskExpr -- cont
  | LitT Lit -- true, false  etc.
  | ReceiveT Int -- copy index 
            Binding -- channel
  | SendT Binding -- channel
         Binding -- data
  -- specific control flow:
  | LoopT Binding Binding TaskExpr -- foreach/map
  | Cond TaskExpr TaskExpr TaskExpr
  -- specific functions:
  | HasSize Binding -- :: [a] -> Bool
  | Size Binding -- :: [a] -> Int
  | Tuple (Either Binding Lit) (Either Binding Lit)
  | Increment Binding -- a = a + 1;
  | Generate Binding Lit -- data generator  
  deriving (Show, Eq, Lift, Generic)

-- This language is intentionally kept very simple and restricted.
data TCExpr
  = Var Binding
  | Apply (App TCExpr)
  | Lambda [Binding]
           TCExpr
  | Let Binding
        TCExpr
        TCExpr
  | Lit Lit
  | Loop TCExpr
  | Channel Int -- num of copies
  | Receive Int -- copy index 
            Binding -- channel
  | Send Binding -- channel
         Binding -- data
  | Run [Task TCExpr] TCExpr -- think let _ = run exprs in result
  deriving (Show, Eq, Lift, Generic)


-------------------- Recursion schemes support --------------------

makeBaseFunctor ''App
makeBaseFunctor ''TCExpr

