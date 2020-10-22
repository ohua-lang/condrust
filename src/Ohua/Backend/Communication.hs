module Ohua.Backend.Communication where

import Ohua.Prelude hiding (Type)

import Ohua.Backend.Lang
import Ohua.Backend.Types


lowerTaskCom :: (Architecture arch, ty ~ Type (Lang arch))
        => arch 
        -> Namespace (TCProgram (Channel ty) (Com 'Recv ty) (TaskExpr ty)) 
        -> Namespace (TCProgram (Channel ty) (TaskExpr ty) (TaskExpr ty))
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

lowerChannels :: (Architecture arch, ty ~ Type (Lang arch))
        => arch 
        -> Namespace (TCProgram (Channel ty) (TaskExpr ty) (TaskExpr ty)) 
        ->  Namespace (TCProgram (Chan arch) (TaskExpr ty) (TaskExpr ty))
lowerChannels arch ns = ns & algos %~ map (\algo -> algo & algoCode %~ convert)
    where 
        convert (TCProgram chans retChan tasks) = 
            TCProgram
                (map (convertChannel arch) chans)
                retChan
                tasks

intoProgram :: Namespace (TCProgram chan retChan (TaskExpr ty)) 
            -> Namespace (Program chan retChan (TaskExpr ty) ty) 
intoProgram ns = ns & algos %~ map (\algo -> algo & algoCode %~ convert)
    where
        convert (TCProgram chans retChan tasks) = 
            Program chans retChan (map createFullTask tasks)

        createFullTask taskExpr = 
            FullTask 
                [s | SendData s <- universe taskExpr]
                [r | ReceiveData r <- universe taskExpr]
                taskExpr
