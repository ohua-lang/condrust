{-# LANGUAGE  TypeOperators #-}
module Ohua.Backend.Types where

import Ohua.Commons.Prelude hiding (Type)

import Ohua.Backend.Lang
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Lazy as HM

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

data TCProgram chan retChan embExpr expr =
    TCProgram
        (NonEmpty chan) -- ^ Channels
        retChan -- ^ Receive on result channel
        [expr] -- TODO (NonEmpty expr) -- ^ Tasks

data Program chan retChan expr embExpr ty =
    Program
        (NonEmpty chan)
        retChan
        [FullTask embExpr ty expr]

data FullTask embExpr ty expr =
    FullTask
        [Com 'Send embExpr ty]
        [Com 'Recv embExpr ty] -- TODO Do not unwrap the binding to understand where state comes from!
        expr
    deriving (Functor)


createFullTask :: TaskExpr embExpr ty -> FullTask embExpr ty (TaskExpr embExpr ty)
createFullTask taskExpr =
    FullTask
        [s | SendData s <- universe taskExpr]
        [r | ReceiveData r <- universe taskExpr]
        taskExpr

taskExpression :: FullTask embExpr ty expr -> expr
taskExpression (FullTask _ _ e) = e


class (Show (Type lang)) => Integration lang where
    type HostModule lang :: *
    type Type lang :: *
    type AlgoSrc lang :: *
    type EmbExpr lang :: *

    type Expr lang :: *
    type Task lang :: *

    convertExpr :: (Architecture arch, Lang arch ~ lang)
                => arch -> TaskExpr (EmbExpr lang) (Type lang) -> Expr lang

    -- TODO I believe now that this function does not belong into the interface anymore!
    lower ::
      ( ErrAndLogM m
      , Architecture arch
      , Lang arch ~ lang
      , ty ~ Type lang
      , embExpr ~ EmbExpr lang)
      => HostModule lang
      -> arch
      -> Namespace (Program (Channel embExpr ty) (Com 'Recv embExpr ty) (TaskExpr embExpr ty) embExpr ty) (AlgoSrc lang) (OhuaType ty 'Resolved)
      -> m (Namespace (Program (Channel embExpr ty) (Com 'Recv embExpr ty) (Task lang) embExpr ty) (AlgoSrc lang) (OhuaType ty 'Resolved)) 

class Architecture arch where
    type Lang arch :: *
    type Chan arch :: *
    type ATask arch :: *

    convertChannel    :: arch -> Channel (EmbExpr (Lang arch)) (Type (Lang arch)) -> Chan arch
    convertRetChannel :: arch -> Channel (EmbExpr (Lang arch)) (Type (Lang arch)) -> Chan arch
    convertRetChannel = convertChannel
    -- TODO implement for sourcing env args when process abstractions can not be closures
    --      this can just be something that works across all implementations!
    -- convertSrcChannel :: arch -> Channel (Type (Lang arch)) -> Chan arch

    convertRecv    :: arch -> Com 'Recv (EmbExpr (Lang arch)) (Type (Lang arch)) -> Expr (Lang arch)
    convertRetRecv :: arch -> Com 'Recv (EmbExpr (Lang arch)) (Type (Lang arch)) -> Expr (Lang arch)
    convertRetRecv = convertRecv

    convertSend:: arch -> Com 'Send (EmbExpr (Lang arch)) (Type (Lang arch)) -> Expr (Lang arch)
    -- TODO implement for sourcing env args when process abstractions can not be closures
    --      this can just be something that works across all implementations!
    -- convertSrcSend    :: arch -> Com 'Recv (Type (Lang arch)) -> Expr (Lang arch)

    build ::
        ( Integration (Lang arch)
        , lang ~ Lang arch
        , ty ~ Type (Lang arch)
        , expr ~ Expr (Lang arch)
        , embExpr ~ EmbExpr (Lang arch)
        , ErrAndLogM m)
        => arch
        -> HostModule lang
        -> Namespace (Program (Chan arch) expr (Task lang) embExpr ty) (AlgoSrc lang) (OhuaType ty 'Resolved)
        -> m (Namespace (Program (Chan arch) expr (ATask arch) embExpr ty) (AlgoSrc lang) (OhuaType ty 'Resolved))

    serialize ::
        ( ErrAndLogM m
        , Integration (Lang arch)
        , lang ~ Lang arch
        , ty ~ Type (Lang arch)
        , expr ~ Expr (Lang arch)
        , embExpr ~ EmbExpr (Lang arch)
        )
        => arch
        -> HostModule lang -- ^ the original module  
        -> HostModule lang -- ^ a helper module encapsulating code we could not copile
        -> Namespace (Program (Chan arch) expr (ATask arch) embExpr ty) (AlgoSrc lang) (OhuaType ty 'Resolved)
        -> m (NonEmpty (FilePath, L.ByteString))


class (Architecture arch) => Transform arch where
  transformTaskExpr :: ( Lang arch ~ lang
                       , Integration lang
                       , ty ~ Type lang
                       , embExpr ~ EmbExpr lang
                       )
                    => HostModule lang
                    -> arch
                    -> FullTask embExpr ty (TaskExpr embExpr ty)
                    -> TaskExpr embExpr ty
  transformTaskExpr _ _ (FullTask _ _ e) = e

  transformTask :: ( Lang arch ~ lang
                   , Integration lang
                   , ty ~ Type lang
                   , embExpr ~ EmbExpr lang
                   )
                => HostModule lang
                -> arch
                -> Task lang
                -> Task lang
  transformTask _ _ = id

updateTaskExprs' :: (expr1 -> expr2)
                 -> Namespace (Program chan recv expr1 embExpr ty)  anno (OhuaType ty 'Resolved)
                 -> Namespace (Program chan recv expr2 embExpr ty)  anno (OhuaType ty 'Resolved)
updateTaskExprs' f ns =
  ns & algos %~ map (\algo -> algo & algoCode %~ go)
  where
   go (Program chans resultChan exprs) =
      Program chans resultChan $ map (f <$>) exprs

updateTaskExprs 
  :: (FullTask embExpr ty expr -> expr)
  -> Namespace (Program (Channel embExpr ty) (Com 'Recv embExpr ty) expr embExpr ty)  anno (OhuaType ty 'Resolved)
  -> Namespace (Program (Channel embExpr ty) (Com 'Recv embExpr ty) expr embExpr ty)  anno (OhuaType ty 'Resolved)
updateTaskExprs f ns =
  ns & algos %~ map (\algo -> algo & algoCode %~ go)
  where
    go (Program chans resultChan exprs) =
      let 
        tasks' = map (\ft@(FullTask s r t) -> FullTask s r $ f ft) exprs
      in
        Program chans resultChan tasks'

updateTasks :: (expr1 -> expr2)
            -> Namespace (Program chan recv expr1 embExpr ty)  anno (OhuaType ty 'Resolved)
            -> Namespace (Program chan recv expr2 embExpr ty)  anno (OhuaType ty 'Resolved)
updateTasks f ns =
  ns & algos %~ map (\algo -> algo & algoCode %~ go)
  where
    go (Program chans resultChan tasks) =
      Program chans resultChan $ map (f <$>) tasks

