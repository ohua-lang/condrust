module Ohua.Backend.TCLang where

import Ohua.Prelude


data Var = Var Text deriving (Show, Eq)

data Task expr = Task expr deriving (Show, Eq)

data App 
  = Stateless QualifiedBinding [Either Var Lit]
  | Stateful Var QualifiedBinding [Either Var Lit]
  deriving (Show, Eq)

-- This language is intentionally kept very simple and restricted.
data TCExpr
  = Binding Var
  | Apply App
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
