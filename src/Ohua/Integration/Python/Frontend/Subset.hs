{-# LANGUAGE DeriveLift, TemplateHaskell #-}
module Ohua.Integration.Python.Frontend.Subset where

import Ohua.Commons.Prelude hiding (Lit)

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated

import Language.Python.Common (SrcSpan)
import qualified Language.Python.Common.AST as Py

{- | This is the supported frontend subset of the python
-- integration, as the AST generated from language-python is underspecified at some points, this
-- subset refers to the grammer definition of python 3.1 which underlies the ASt
-}


data PythonType = PythonType deriving (Show, Eq, Generic)

newtype Suite  =  PySuite [Stmt]
  deriving (Eq, Generic, Show)

type Block = [Stmt] -- ^ this is an alias for blocks inside loops or branches that must not contain 'return'

data Stmt  =
    WhileStmt Expr Block
    | ForStmt Target Expr Block
    | CondStmt [(Expr, Block )] (Maybe Block)
    -- TODO: Rework when/if assignments and returns of lists of targets are supported
    | Assign Target Expr
    | Pass
    | StmtExpr Expr
    -- TODO: Add AugmAssign when possible
    | Return (Maybe Expr)
      deriving (Eq, Generic, Show)


data Expr =
    Var Binding
    | Int Integer
    | Bool Bool
    | None
    | PyLiteral (Py.Expr SrcSpan)
    -- MethodCalls are not a separate token. They are 'Calls' where:
    -- A) the call_fun attribute is a dotExpr and
    -- B) it is not a class method or a toplevel function of an imported module
    | Call FRef [Argument]
    | CondExpr Expr Expr Expr
    | BinaryOp BinOp Expr Expr
    | UnaryOp UnOp Expr
    | Lambda [Param] Expr
    | Tuple [Expr]
    -- for the following expressions, there's no buildin equivalent in Ohua's frontend
    -- so we will map them to fuction calls (because also in Python they are actually syntax sugar)
    | List [Expr]
    -- | ListComp Context Expr Target Expr -- ^ x = [f(t) for t in ts] gives ListComp x f(t) t ts
    | Dict [(Expr, Expr)]
    | Set [Expr]
    | Subscript Binding Expr
    -- TODO: test this bevor it's included
    -- \| ParenExpr
    deriving (Eq, Show)

-- In the AST of Rust as well as Python the first part of a Call expression is just an expression
-- this allows things like ' x = [a,b,c]()' i.e. Calls on any expressions 
-- Specifying FRef doesn't fully solve this, but at least makes a distinction in the conversion
-- of callable vs. non-callable expressions
data FRef = Pure Binding | Dotted Binding QualifiedBinding | Direct Expr deriving (Show, Eq, Generic)
data BinOp =  
    Plus | Minus | Multiply | Divide | FloorDivide | Modulo | Exponent | MatrixMult | 
    And | Or | Is | IsNot | In | NotIn | 
    LessThan | GreaterThan | Equality | GreaterThanEquals | LessThanEquals | NotEquals |
    BinaryAnd | BinaryOr | Xor | ShiftLeft | ShiftRight deriving (Show, Eq, Generic)

data UnOp =  Not | Invert  deriving (Show, Eq, Generic)

data Argument = Arg Expr deriving (Eq, Show) -- StarArg Expr | StarKwArg Expr | KwArg Expr Ident  

-- Note: first Maybe is an optional type annotation, second Maybe is the default
data Param = Param Binding deriving (Eq, Show) {-- (Maybe Expr) (Maybe Expr)| ArgsParam Binding | KwargsParam Binding -}

type Context = Binding -- ^ We need to keep track of the variable name a coprehension is assigned to 
  
    -- Todo: Call that TupP, ListP or have both?
    -- Reason: On the python side patterns can be lists or
    -- tuples 
    -- [1,2,3] = fun()
    --- 1,2,3 = fun()
    -- > How is this different in the AST
    -- > Do I loos meaning when I map both to the IR TupP ?
    -- TODO: Check if Rust now supports subscriptions to attributes or slices
    -- TODO: Should I introduce a separate symbol for '_' Bindings?
    -- TODO: Dots can also be patterns but we currently do not support attribute access

-- Problem : this is actually a lie, because Subcr are only supported for Assignment, not for ForLoop
data Target = Single Binding | Tpl [Binding] | Subscr Expr  deriving (Show, Eq, Generic)

-- TODO: Comment in when I have augmented assignments
{-data AssignOps = 
    PlusAssign
    | MinusAssign 
    | MultAssign 
    | DivAssign 
    | ModAssign 
    | PowAssign
    | BinAndAssign 
    | BinOrAssign
    | BinXorAssign 
    | LeftShiftAssign
    | RightShiftAssign
    | FloorDivAssign
    | MatrixMultAssign-}



-------------------- Recursion schemes support --------------------

-- Question: What happens here ?!

makeBaseFunctor ''Stmt
makeBaseFunctor ''Suite

instance Plated Stmt  where plate = gplate
instance Plated Suite where plate = gplate

