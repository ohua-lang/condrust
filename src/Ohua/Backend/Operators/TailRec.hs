{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass #-}
module Ohua.Backend.Operators.TailRec where

import Ohua.Prelude

import Ohua.Backend.Lang

import Data.Either

data RecFun ty where
    RecFun
        :: Com 'Channel ty
        -> Com 'Channel ty
        -- I'm loosing the type info here because getting the instances below is a pain otherwise :(
        -> [Com 'Channel ty]
        -> [Either (Com 'Recv ty) (Lit ty)]
        -> [Com 'Recv ty]
        -> Com 'Recv ty
        -> Com 'Recv ty
        -> RecFun ty

deriving instance Generic (RecFun ty)
deriving instance Eq (RecFun ty)
deriving instance Hashable (RecFun ty)

mkRecFun :: RecFun ty -> TaskExpr ty
mkRecFun (RecFun resultOut ctrlOut recArgsOuts recInitArgsIns recArgsIns recCondIn recResultIn) =
    EndlessLoop $
        dispatchCtrlSig contSig $
        -- here I would have loved to have the type safety again :(
        recvInits $
        sendInits $
        Stmt
            (While condRecv $
                Stmt resultRecv $
                dispatchCtrlSig contSig $
                recvLoopArgs $
                sendLoopArgs $
                Lit UnitLit ) $
            dispatchCtrlSig stopSig $
            Let "finaResult" resultRecv $
            SendData $ SSend resultOut "finalResult"
    where
        contSig = Tuple (Right $ BoolLit True) (Right $ NumericLit 1)
        stopSig = Tuple (Right $ BoolLit False) (Right $ NumericLit 0)
        dispatchCtrlSig c sig =
                Let "ctrlSig" sig $
                    Stmt
                        (SendData $ SSend ctrlOut "ctrlSig")
                        c
        recvInits c = foldr (\(v,r) c' -> case r of
                                Left r' -> Let v (ReceiveData r') c'
                                Right l -> Lit l)
                            c
                            initArgsRecv
        sendInits c = foldr (\(o,v) c' -> Stmt (SendData $ SSend o v)  c') c $ zip recArgsOuts $ map fst initArgsRecv
        initArgsRecv = zipWith (curry (first (("init_" <>) . show))) [0..] recInitArgsIns
        condRecv = ReceiveData recCondIn
        resultRecv = ReceiveData recResultIn
        loopArgsRecv = zipWith (curry (first (("loop_res_" <>) . show))) [0..] recArgsIns
        recvLoopArgs c = foldr (\(v,r) c' -> Let v (ReceiveData r) c') c loopArgsRecv
        sendLoopArgs c = foldr (\(o,v) c' -> Stmt (SendData $ SSend o v)  c') c $ zip recArgsOuts $ map fst loopArgsRecv
