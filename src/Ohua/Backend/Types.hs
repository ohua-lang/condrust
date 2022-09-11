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
    type HostModule lang :: *
    type Type lang :: *
    type AlgoSrc lang :: *

    type Expr lang :: *
    type Task lang :: *

    convertExpr :: (Architecture arch, Lang arch ~ lang)
                => arch -> TaskExpr (Type lang) -> Expr lang

    -- TODO I believe now that this function does not belong into the interface anymore!
    lower ::
      ( CompM m
      , Architecture arch
      , Lang arch ~ lang
      , ty ~ Type lang)
      => HostModule lang
      -> arch
      -> Namespace (Program (Channel ty) (Com 'Recv ty) (TaskExpr ty) ty) (AlgoSrc lang) ty
      -> m (Namespace (Program (Channel ty) (Com 'Recv ty) (Task lang) ty) (AlgoSrc lang) ty) 

class Architecture arch where
    type Lang arch :: *
    type Chan arch :: *
    type ATask arch :: *

    convertChannel :: arch -> Channel (Type (Lang arch)) -> Chan arch
    convertRecv :: arch -> Com 'Recv (Type (Lang arch)) -> Expr (Lang arch)
    convertSend:: arch -> Com 'Send (Type (Lang arch)) -> Expr (Lang arch)

    build ::
        ( Integration (Lang arch)
        , lang ~ Lang arch
        , ty ~ Type (Lang arch)
        , expr ~ Expr (Lang arch)
        , CompM m)
        => arch
        -> HostModule lang
        -> Namespace (Program (Chan arch) expr (Task lang) ty) (AlgoSrc lang) ty
        -> m (Namespace (Program (Chan arch) expr (ATask arch) ty) (AlgoSrc lang) ty)

    serialize ::
        ( CompM m
        , Integration (Lang arch)
        , lang ~ Lang arch
        , ty ~ Type (Lang arch)
        , expr ~ Expr (Lang arch)
        )
        => arch
        -> HostModule lang -- ^ the original module  
        -> HostModule lang -- ° a helper module encapsulating code we could not copile
        -> Namespace (Program (Chan arch) expr (ATask arch) ty) (AlgoSrc lang) ty
        -> m (NonEmpty (FilePath, L.ByteString))


class (Architecture arch) => Transform arch where
  transformTaskExpr :: ( Lang arch ~ lang
                       , Integration lang
                       , ty ~ Type lang
                       )
                    => HostModule lang
                    -> arch
                    -> TaskExpr ty
                    -> TaskExpr ty
  transformTaskExpr _ _ = id

  transformTask :: ( Lang arch ~ lang
                   , Integration lang
                   , ty ~ Type lang
                   )
                => HostModule lang
                -> arch
                -> Task lang
                -> Task lang
  transformTask _ _ = id


updateTaskExprs :: (expr1 -> expr2)
                -> Namespace (TCProgram chan recv expr1) anno ty
                -> Namespace (TCProgram  chan recv expr2) anno ty
updateTaskExprs f ns =
  ns & algos %~ map (\algo -> algo & algoCode %~ go)
  where
    go (TCProgram chans resultChan exprs) =
      TCProgram chans resultChan $ map f exprs

updateTasks :: (expr1 -> expr2)
            -> Namespace (Program chan recv expr1 ty) anno ty
            -> Namespace (Program  chan recv expr2 ty) anno ty
updateTasks f ns =
  ns & algos %~ map (\algo -> algo & algoCode %~ go)
  where
    go (Program chans resultChan tasks) =
      Program chans resultChan $ map (f <$>) tasks

