module Ohua.Backend.Communication where

import Ohua.Prelude hiding (Type)

import Ohua.Backend.Lang
import Ohua.Backend.Types


lowerRetCom :: 
        ( Architecture arch
        , ty ~ Type (Lang arch)
        , retChan ~ Expr (Lang arch))
        => arch 
        -> Namespace (TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)) anno
        -> Namespace (TCProgram (Channel ty) retChan (TaskExpr ty)) anno
lowerRetCom arch ns = ns & algos %~ map (\algo -> algo & algoCode %~ convertCommunication)
    where 
        convertCommunication (TCProgram chans retChan tasks) = 
            TCProgram
                chans
                (convertRecv arch retChan)
                tasks
        assignType = undefined -- Really, we need to attach the Rust code to the algo!

lowerChannels :: 
        ( Architecture arch
        , ty ~ Type (Lang arch)
        , retChan ~ Expr (Lang arch))
        => arch 
        -> Namespace (TCProgram (Channel ty) retChan (TaskExpr ty)) anno
        -> Namespace (TCProgram (Chan arch) retChan (TaskExpr ty)) anno
lowerChannels arch ns = ns & algos %~ map (\algo -> algo & algoCode %~ convert)
    where 
        convert (TCProgram chans retChan tasks) = 
            TCProgram
                (map (convertChannel arch) chans)
                retChan
                tasks

intoProgram :: Namespace (TCProgram chan retChan (TaskExpr ty)) anno
            -> Namespace (Program chan retChan (TaskExpr ty) ty) anno
intoProgram ns = ns & algos %~ map (\algo -> algo & algoCode %~ convert)
    where
        convert (TCProgram chans retChan tasks) = 
            Program chans retChan (map createFullTask tasks)

        createFullTask taskExpr = 
            FullTask 
                [s | SendData s <- universe taskExpr]
                [r | ReceiveData r <- universe taskExpr]
                taskExpr
