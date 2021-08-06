{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ohua.Backend.Operators.TailRec where

import Data.Either
import Ohua.Backend.Lang
import Ohua.Prelude

data RecFun ty where
  RecFun ::
    Com 'Channel ty ->
    Maybe (Com 'Channel ty) ->
    -- I'm loosing the type info here because getting the instances below is a pain otherwise :(
    [Com 'Channel ty] ->
    [Either (Com 'Recv ty) (Lit ty)] ->
    [Com 'Recv ty] ->
    Com 'Recv ty ->
    Com 'Recv ty ->
    RecFun ty

deriving instance Generic (RecFun ty)

deriving instance Eq (RecFun ty)

deriving instance Hashable (RecFun ty)

finalResult = "finalResult"

mkRecFun :: RecFun ty -> TaskExpr ty
mkRecFun (RecFun resultOut ctrlOut recArgsOuts recInitArgsIns recArgsIns recCondIn recResultIn) =
  loop $
    dispatchCtrlSig contSig $
      -- here I would have loved to have the type safety again :(
      recvInits $
        sendInits $
          Stmt
            ( While condRecv $
                Stmt resultRecv $
                  dispatchCtrlSig contSig $
                    recvLoopArgs $
                      sendLoopArgs $
                        Lit UnitLit
            )
            $ dispatchCtrlSig stopSig $
              Let finalResult resultRecv $
                SendData $ SSend resultOut finalResult
  where
    -- this loop stuff may go away once we start fusing recur
    loop c = case filter isLeft recInitArgsIns of
      [] -> c
      _ -> EndlessLoop c
    contSig = Tuple (Right $ BoolLit True) (Right $ NumericLit 1)
    stopSig = Tuple (Right $ BoolLit False) (Right $ NumericLit 0)
    dispatchCtrlSig sig c =
      case ctrlOut of
        Just cOut ->
          Let "ctrlSig" sig $
            Stmt
              (SendData $ SSend cOut "ctrlSig")
              c
        _ -> c
    recvInits c =
      foldr
        ( \(v, r) c' ->
            Let
              v
              ( case r of
                  Left r' -> ReceiveData r'
                  Right l -> Lit l
              )
              c'
        )
        c
        initArgsRecv
    sendInits c =
      foldr
        (\(o, v) c' -> Stmt (SendData $ SSend o v) c')
        c
        $ zip recArgsOuts $
          map fst initArgsRecv
    initArgsRecv = zipWith (curry (first (("init_" <>) . show))) [0 ..] recInitArgsIns
    condRecv = ReceiveData recCondIn
    resultRecv = ReceiveData recResultIn
    loopArgsRecv = zipWith (curry (first (("loop_res_" <>) . show))) [0 ..] recArgsIns
    recvLoopArgs c = foldr (\(v, r) c' -> Let v (ReceiveData r) c') c loopArgsRecv
    sendLoopArgs c =
      foldr (\(o, v) c' -> Stmt (SendData $ SSend o v) c') c $
        zip recArgsOuts $
          map fst loopArgsRecv
