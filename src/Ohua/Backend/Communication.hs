module Ohua.Backend.Communication where

import Ohua.Prelude hiding (Type)

import Ohua.Backend.Lang
import Ohua.Backend.Types


lowerRetCom ::
        ( Architecture arch
        , ty ~ Type (Lang arch)
        , task ~ Task (Lang arch)
        , retChan ~ Expr (Lang arch))
        => arch
        -> Namespace (Program (Channel ty) (Com 'Recv ty) task ty) anno ty
        -> Namespace (Program (Channel ty) retChan task ty) anno ty
lowerRetCom arch ns = ns & algos %~ map (\algo -> algo & algoCode %~ convertCommunication)
    where
        convertCommunication (Program chans retChan tasks) =
            Program
                chans
                (convertRecv arch retChan)
                tasks

lowerChannels ::
        ( Architecture arch
        , ty ~ Type (Lang arch)
        , task ~ Task (Lang arch)
        , retChan ~ Expr (Lang arch))
        => arch
        -> Namespace (Program (Channel ty) retChan task ty) anno ty
        -> Namespace (Program (Chan arch) retChan task ty) anno ty
lowerChannels arch ns = ns & algos %~ map (\algo -> algo & algoCode %~ convert)
    where
        convert (Program chans retChan tasks) =
            Program
                (map (convertChannel arch) chans)
                retChan
                tasks

intoProgram :: Namespace (TCProgram chan retChan (TaskExpr ty)) anno ty
            -> Namespace (Program chan retChan (TaskExpr ty) ty) anno ty
intoProgram ns = ns & algos %~ map (\algo -> algo & algoCode %~ convert)
    where
        convert (TCProgram chans retChan tasks) =
            Program chans retChan (map createFullTask tasks)

        createFullTask taskExpr =
            FullTask
                [s | SendData s <- universe taskExpr]
                [r | ReceiveData r <- universe taskExpr]
                taskExpr
