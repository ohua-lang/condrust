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

