{-# LANGUAGE TemplateHaskell #-}
module Ohua.Integration.Python.Backend.Subset where

import Ohua.Prelude 

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated

{-| In contrast to the frontend subset, the backend subset is restricted by the language of the Ohua backend. Hence I use (mostly) a subset
 of the AST from language-python although it allows invalid python syntax-}
type Suite = [Stmt]

data Stmt = 
    WhileStmt Expr Suite -- original: While Expr Suite Suite, we dont't have the 'else-suite'
    | ForStmt [Expr] Expr Suite -- original: For [Expr] Suite Suite, we dont't have the 'else-suite'
    | CondStmt (Expr, Suite) Suite -- original: Conditional [(Expr, Suite)] Suite, we dont't have a list of elifs but only if -suite- suite
    | StmtExpr Expr
    -- TODO: Check what kinds of target actually arrive here
    | Assign [Expr] Expr 
    deriving (Eq, Generic, Show)

data Expr = 
    Int Integer
    | Bool Bool
    | None 
    | Var Binding 
    | Strings [String]
    -- There is no explicit MethodCall in Python
    -- A MethodCall in Python Syntax is a Call, whose call_fun attribute is a DotExpr
    | Call Expr [Argument]
    | DotExpr Expr QualifiedBinding
    | CondExpr Expr Expr Expr
    | Tuple [Expr] 
    | List [Expr]
    | Dict [(Expr, Expr)]
    | Set [Expr]
    -- TODO: Subscript in the python AST does not 
    -- make a difference between tuples and lists
    -- So actually I use the 'universal' subscript expr,
    -- but only for the first and second element of tuples falling out 
    -- of the compiler backend  
    | TplSubscript Binding Integer 
    | BinaryOp BinOp Expr Expr
    | UnaryOp UnOp Expr
    deriving (Eq, Generic , Show)


data BinOp =  
    Plus | Minus | Multiply | Divide | FloorDivide | Modulo | Exponent | MatrixMult | 
    And | Or | Is | IsNot | In | NotIn | 
    LessThan | GreaterThan | Equality | GreaterThanEquals | LessThanEquals | NotEquals |
    BinaryAnd | BinaryOr | Xor | ShiftLeft | ShiftRight deriving (Show, Eq, Generic)

data UnOp =  Not | Invert  deriving (Show, Eq, Generic)

data Argument = Arg Expr deriving (Eq, Show) -- StarArg Expr | StarKwArg Expr | KwArg Expr Ident

-------------------- Recursion schemes support --------------------

makeBaseFunctor ''Expr

instance Plated Expr where plate = gplate

transformExprInSuite :: (Expr -> Expr) -> Suite -> Suite
transformExprInSuite func  = runIdentity . transformExprInSuiteM (pure . func)

transformExprInSuiteM :: (Monad m) => (Expr -> m Expr) -> Suite -> m Suite
transformExprInSuiteM func = transfSuite
    where
        transfSuite stmts = reverse <$> mapM transfStmt (reverse stmts)

        transfStmt (WhileStmt expr suite) = WhileStmt expr <$> transfSuite suite
        transfStmt (ForStmt exprs expr suite) =  
        transfStmt (CondStmt (ife, ifsuite) elsesuite) = undefined 
        transfStmt (Assign exprs expr) = undefined 
        transfStmt (StmtExpr expr) = undefined


