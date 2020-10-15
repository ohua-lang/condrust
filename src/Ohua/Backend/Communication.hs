module Ohua.Backend.Communication where

import Ohua.Prelude

import Ohua.Backend.Lang
import Ohua.Backend.Types


lowerTaskCom :: ConvertTaskCom arch 
        => arch 
        -> Namespace (TCProgram Channel (Com 'Recv) TaskExpr) 
        -> Namespace (TCProgram Channel TaskExpr TaskExpr)
lowerTaskCom arch ns = ns & algos %~ map (\algo -> algo & algoCode %~ convertCommunication)
    where 
        convertCommunication (TCProgram chans retChan tasks) = 
            TCProgram
                chans
                (convertRecv arch retChan)
                (map convertRecvSend tasks)
        convertRecvSend = transform $ \case
            ReceiveData r -> convertRecv arch r
            SendData s -> convertSend arch s
            e -> e

lowerChannels :: Architecture arch
        => arch 
        -> Namespace (TCProgram Channel TaskExpr TaskExpr) 
        ->  Namespace (TCProgram (Chan arch) TaskExpr TaskExpr)
lowerChannels arch ns = ns & algos %~ map (\algo -> algo & algoCode %~ convert)
    where 
        convert (TCProgram chans retChan tasks) = 
            TCProgram
                (map (convertChannel arch) chans)
                retChan
                tasks

intoProgram :: Namespace (TCProgram chan retChan TaskExpr) 
            -> Namespace (Program chan retChan TaskExpr) 
intoProgram ns = ns & algos %~ map (\algo -> algo & algoCode %~ convert)
    where
        convert (TCProgram chans retChan tasks) = 
            Program chans retChan (map createFullTask tasks)

        createFullTask taskExpr = 
            FullTask 
                [s | SendData s <- universe taskExpr]
                [r | ReceiveData r <- universe taskExpr]
                taskExpr
