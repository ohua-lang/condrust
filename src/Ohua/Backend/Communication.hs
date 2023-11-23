{-# LANGUAGE  TypeOperators #-}
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
        -> Namespace (Program (Chan arch) (Com 'Recv ty) task ty) anno (OhuaType ty 'Resolved)
        -> Namespace (Program (Chan arch) retChan        task ty) anno (OhuaType ty 'Resolved)
lowerRetCom arch ns = ns & algos %~ map (\algo -> algo & algoCode %~ convertCommunication)
    where
        convertCommunication (Program chans retChan tasks) =
            Program
                chans
                (convertRetRecv arch retChan)
                tasks

lowerChannels ::
        ( Architecture arch
        , ty ~ Type (Lang arch)
        , task ~ Task (Lang arch))
        => arch
        -> Namespace (Program (Channel ty) (Com 'Recv ty) task ty) anno (OhuaType ty 'Resolved)
        -> Namespace (Program (Chan arch)  (Com 'Recv ty) task ty) anno (OhuaType ty 'Resolved)
lowerChannels arch ns = ns & algos %~ map (\algo -> algo & algoCode %~ convert)
    where
        convert (Program chans retChan tasks) =
            Program
                (map (convertChan retChan) chans)
                retChan
                tasks
        convertChan retChan chan | retChan == chan = convertRetChannel arch chan
        convertChan _ chan = convertChannel arch chan


intoProgram :: Namespace (TCProgram chan retChan (TaskExpr ty)) anno (OhuaType ty 'Resolved)
            -> Namespace (Program   chan retChan (TaskExpr ty) ty) anno (OhuaType ty 'Resolved)
intoProgram ns = ns & algos %~ map (\algo -> algo & algoCode %~ convert)
    where
        convert (TCProgram chans retChan tasks) =
            Program chans retChan (map createFullTask tasks)


