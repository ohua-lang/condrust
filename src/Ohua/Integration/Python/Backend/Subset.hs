{-# LANGUAGE TemplateHaskell #-}
module Ohua.Integration.Python.Backend.Subset where

import Ohua.Commons.Prelude 

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax ()
import Control.Lens.Plated

import Language.Python.Common (SrcSpan)
import qualified Language.Python.Common.AST as Py


{-| In contrast to the frontend subset, the backend subset is restricted by the language of the Ohua backend. Hence I use (mostly) a subset
 of the AST from language-python although it allows invalid python syntax-}
type Suite = [Stmt]

data Stmt = 
    WhileStmt Expr Suite -- original: While Expr Suite Suite, we dont't have the 'else-suite'
    | ForStmt [Expr] Expr Suite -- original: For [Expr] Suite Suite, we dont't have the 'else-suite'
    | CondStmt (Expr, Suite) Suite -- original: Conditional [(Expr, Suite)] Suite, we dont't have a list of elifs but only if -suite- suite
    -- TODO: Check what kinds of target actually arrive here
    | Assign [Expr] Expr 
    -- Actually there never is a StmtExpr, we just need it as an intermediate 
    -- wrapper for expressions because convertExpr 
    -- may not return statments OR exprs
    | StmtExpr Expr
    deriving (Eq, Generic, Show)

data Expr = 
    Int Integer
    | Bool Bool
    | None 
    | Var Binding 
    | Strings [String]
    | PyLiteral (Py.Expr SrcSpan)
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
    | Subscript Expr Expr 
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
makeBaseFunctor ''Stmt
instance Plated Expr where plate = gplate
instance Plated Stmt where plate = gplate

transformExprInSuite :: (Stmt -> Stmt) -> Suite -> Suite
transformExprInSuite func  = runIdentity . transformExprInSuiteM (pure . func)

transformExprInSuiteM :: (Monad m) => (Stmt -> m Stmt) -> Suite -> m Suite
transformExprInSuiteM func = transfSuite
    where
        transfSuite stmts = reverse <$> mapM (transformM (func <=< transfStmt)) (reverse stmts)

        transfStmt (WhileStmt expr suite) = WhileStmt expr <$> transfSuite suite
        transfStmt (ForStmt exprs expr suite) = ForStmt exprs expr <$> transfSuite suite
        -- TODO: Check why the backend else in Rust is not transformed
        transfStmt (CondStmt (ife, ifsuite) elsesuite) = 
            (\suite -> CondStmt (ife, suite) elsesuite) <$> transfSuite ifsuite
        -- In Rust, assignments can contain blocks, 
        -- in Python the assigned expression can really only be one expression
        transfStmt (Assign exprs expr) = pure $ Assign exprs expr
        transfStmt (StmtExpr expr) = pure $ StmtExpr expr

    
        


