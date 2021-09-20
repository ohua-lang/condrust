{-# LANGUAGE DeriveLift, TemplateHaskell #-}
module Ohua.Integration.Python.Frontend.Subset where

import Ohua.Prelude hiding (Lit)

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Language.Haskell.TH.Syntax (Lift)
import Control.Lens.Plated

-- | This is the supported frontend subset of the python 
-- integration

-- I currently don't type anything but for a) the possibility to use at least annotated
-- types and b) consistency with other integration I'll thread this dummmy trough the logic
data PythonType = PythonObject deriving (Show, Eq, Generic)

newtype Suite  =  PySuite [Stmt]
  deriving (Eq, Generic, Show)

data Stmt  = 
    -- TODO: need to specify that only while and for
    --  with empty elseBlock are supported
    WhileStmt Expr Suite 
    -- Question: Do i express the fact, that we do not support 
    -- else-branches for While/For by leaving out the according part
    -- of the original constructor ? 
    | ForStmt [Expr] Expr Suite 
    | CondStmt [(Expr, Suite )] Suite 
    -- TODO: Rework when/if assignments and returns of lists of targets are supported
    | Assign [Expr] Expr 
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
    -- MethodCalls are not a separete token. They are 'Calls' where:
    -- A) the call_fun attribute is a dotExpr and 
    -- B) it is not a class method or a toplevel function of an imported module
    -- > We don't see the implicit 'self' argument in method calls, So the question is:
    -- (How) can we distiguish method calls on statefull objects 
    -- from calls to functions in other namespaces :-/
    | Call FRef [Argument]
    | CondExpr Expr Expr Expr 
    | BinaryOp BinOp Expr Expr
    | UnaryOp UnOp Expr
    -- TODO: in Dot =>  Expr = Var Ident | Dot DotExpr Ident 
    -- Question: Appart from method calls, Dot's also represent attribute access. 
    -- This is equivalent to FieldAccess in Rust, so we do not generally support it.
    -- So is it a part of the subset or not? 
    -- | Dot QualifiedBinding 
    | Tuple [Expr]
    -- TODO: test this bevore it's included
    -- \| ParenExpr
    deriving (Eq, Show)

data FRef = Pure Binding | Dotted Binding QualifiedBinding deriving (Show, Eq, Generic)
data BinOp =  
    Plus | Minus | Multiply | Divide | FloorDivide | Modulo | Exponent | MatrixMult | 
    And | Or | Is | IsNot | In | NotIn | 
    LessThan | GreaterThan | Equality | GreaterThanEquals | LessThanEquals | NotEquals |
    BinaryAnd | BinaryOr | Xor | ShiftLeft | ShiftRight deriving (Show, Eq, Generic)

data UnOp =  Not | Invert  deriving (Show, Eq, Generic)

data Argument = Arg Expr deriving (Eq, Show) -- StarArg Expr | StarKwArg Expr | KwArg Expr Ident  

data Param = Param {-- ArgsParam | KwargsParam-}

data Pat = 
    VarP Binding
    -- Todo: Call that TupP, ListP or have both?
    -- Reason: On the python side patterns can be lists or
    -- tuples 
    -- [1,2,3] = fun()
    --- 1,2,3 = fun()
    -- > How is this different in the AST
    -- > Do I loos meaning when I map both to the IR TupP ?
    | TupP [Binding]
    -- TODO: Check if Rust now supports subscriptions to attributes or slices
    -- TODO: Should I introduce a separate symbol for '_' Bindings?

    -- TODO: Dots can also be patterns but we currently do not support attribute access
    -- DotP QualifiedBinding
    deriving (Show, Eq, Generic)

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

makeBaseFunctor ''Pat
makeBaseFunctor ''Stmt
makeBaseFunctor ''Suite

instance Plated Pat where plate = gplate
instance Plated Stmt  where plate = gplate
instance Plated Suite where plate = gplate

