module Ohua.Backend.Lang where

import Ohua.Prelude


newtype Var = Var Text deriving (Show, Eq)

newtype Task expr = Task expr deriving (Show, Eq)

data App expr
  = Stateless QualifiedBinding [expr]
  | Stateful Var QualifiedBinding [expr]
  deriving (Show, Eq)

-- This language is intentionally kept very simple and restricted.
data TCExpr
  = Binding Var
  | Apply (App TCExpr)
  | Lambda [Var]
           TCExpr
  | Let Var
        TCExpr
        TCExpr
  | Lit Lit
  | Loop TCExpr
  | Channel Int -- num of copies
  | Receive Int -- copy index 
            Var -- channel
  | Send Var -- channel
         Var -- data
  | Run [Task TCExpr] TCExpr -- think let _ = run exprs in result
  deriving (Show, Eq)
