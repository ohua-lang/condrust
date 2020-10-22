module Ohua.Backend.Types where

import Ohua.Prelude hiding (Type)

import Ohua.Backend.Lang
import qualified Data.ByteString.Lazy.Char8 as L


-- data TaskType = PureTask | STTask

-- data TaskFun :: TaskType -> Type -> Type where
--     SPureTask :: [Com 'Send] -> expr -> [Com 'Recv] -> TaskFun 'PureTask expr 
--     SSTTask :: (Maybe (Com 'Send), Maybe (Com 'Send)) -> expr -> Com 'Recv -> [Com 'Recv] -> TaskFun 'STTask expr

-- deriving instance Functor (TaskFun exprType)

-- data TCProgram chan retChan expr = 
--     forall (exprType::TaskType). (Show (TaskFun expr exprType), Eq (TaskFun expr exprType)) =>
--         TCProgram 
--         (NonEmpty chan) -- ^ Channels
--         retChan -- ^ Receive on result channel
--         [TaskFun expr exprType] -- TODO (NonEmpty expr) -- ^ Tasks
--         -- [Function expr] -- ^ Functions

-- deriving instance (Show chan, Show retChan, Show expr) => Show (TCProgram chan retChan expr)
-- deriving instance (Eq chan, Eq retChan) => Eq (TCProgram chan retChan expr)

data TCProgram chan retChan expr = 
    TCProgram 
        (NonEmpty chan) -- ^ Channels
        retChan -- ^ Receive on result channel
        [expr] -- TODO (NonEmpty expr) -- ^ Tasks

data Program chan retChan expr ty = 
    Program
        (NonEmpty chan)
        retChan
        [FullTask ty expr]

data FullTask ty expr = 
    FullTask 
        [Com 'Send ty] 
        [Com 'Recv ty] -- TODO Do not unwrap the binding to understand where state comes from!
        expr
    deriving (Functor)

taskExpression :: FullTask ty expr -> expr
taskExpression (FullTask _ _ e) = e

class Integration lang where
    type NS lang :: *
    type Type lang :: *

    type RetChan lang :: *
    type Expr lang :: *
    type Task lang :: *

    convertExpr :: (Architecture arch, Lang arch ~ lang) => arch -> TaskExpr (Type lang) -> Expr lang

    lower :: 
        ( CompM m
        , Architecture arch
        , Lang arch ~ lang
        , ty ~ (Type lang))
        => NS lang
        -> arch
        -> Namespace (Program (Chan arch) (TaskExpr ty) (TaskExpr ty) ty)
        -> m (Namespace (Program (Chan arch) (RetChan lang) (Task lang) ty))

class (ConvertTaskCom arch) => Architecture arch where
    type Lang arch :: *
    type Chan arch :: *
    type ARetChan arch :: *
    type ATask arch :: *

    convertChannel :: arch -> Channel (Type (Lang arch)) -> Chan arch

    build :: 
        ( Integration (Lang arch)
        , lang ~ (Lang arch)
        , ty ~ (Type (Lang arch))
        , CompM m)
        => arch
        -> NS lang
        -> Namespace (Program (Chan arch) (RetChan lang) (Task lang) ty)
        -> m (Namespace (Program (Chan arch) (ARetChan arch) (ATask arch) ty))

    serialize :: 
        ( CompM m
        , Integration (Lang arch)
        , lang ~ (Lang arch)
        , ty ~ (Type (Lang arch)))
        => arch
        -> NS lang
        -> Namespace (Program (Chan arch) (ARetChan arch) (ATask arch) ty)
        -> m (NonEmpty (FilePath, L.ByteString))

class ConvertTaskCom arch where
    convertRecv :: arch -> Com 'Recv ty -> TaskExpr ty
    convertSend:: arch -> Com 'Send ty -> TaskExpr ty
