module Ohua.Backend.TCLang where

data Var = Var Text

data Loop expr = Loop expr

data Task expr = Expr expr | LoopExpr expr

data App 
  = Stateless QualifiedBinding [Either Var Lit]
  | Stateful Var QualifiedBinding [Either Var Lit]

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
  | Channel Int -- num of copies
  | Receive Int -- copy index 
            Var -- channel
  | Send Var -- channel
         Var -- data
  | TList [Task TCExpr]
  deriving (Show, Typeable, Data, Eq)
