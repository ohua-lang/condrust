module Ohua.Integration.Python.Backend.Subset where

import Ohua.Prelude 
newtype Suite anno =  PySuite [Stmt anno]

data Stmt anno = 
    WhileStmt 
    | ForStmt 
    | CondStmt 
    | StmtExpr 
    | Assign 
    | AugmentedAssign
    deriving (Eq, Generic, Functor, Foldable, Traversable, Show)

data Expr = 
    Int 
    | Bool 
    | None 
    | Var 
    -- There is no explicit MethodCall in Python
    -- A MethodCall in Python Syntax is a Call, whose call_fun attribute is a DotExpr
    | Call 
    | DotExpr  
    | ArgExpr 
    | CondExpr 
    | Tuple
    -- TODO: Subscript in the python ASt does not 
    -- make a difference between tuples and lists
    -- So actually I use the 'universal' subscript expr,
    -- but only for the first and second element of tuples falling out 
    -- of the compiler backend  
    | TplSubscript
    | AssignOp 
    | Not 
    | BinaryOp
    | UnaryOp 
    deriving (Eq, Generic)

