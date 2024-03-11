{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ohua.Backend.Operators.TailRec where

import Data.Either
import Ohua.Backend.Lang
import Ohua.Commons.Prelude
import Ohua.Backend.Operators.Common (ctrlTuple)

data RecFun embExpr annot ty where
  RecFun ::
    Com 'Channel embExpr annot ty ->
    Maybe (Com 'Channel embExpr annot ty) ->
    -- I'm loosing the type info here because getting the instances below is a pain otherwise :(
    [Com 'Channel embExpr annot ty] ->
    [Either (Com 'Recv embExpr annot ty) (Lit embExpr annot ty Resolved)] ->
    [Com 'Recv embExpr annot ty] ->
    Com 'Recv embExpr annot ty ->
    Com 'Recv embExpr annot ty ->
    RecFun embExpr annot ty

deriving instance Generic (RecFun embExpr annot ty)

deriving instance Eq (RecFun embExpr annot ty)

deriving instance Hashable (RecFun embExpr annot ty)

deriving instance Show (RecFun embExpr annot ty)

finalResult = "finalResult"

mkRecFun :: RecFun embExpr annot ty -> TaskExpr embExpr annot ty
mkRecFun (RecFun resultOut ctrlOut recArgsOuts recInitArgsIns recArgsIns recCondIn recResultIn) =
  let innerBody =
        dispatchCtrlSig contSig $
          recvLoopArgs $
            sendLoopArgs $
              Lit UnitLit
      loopBody = case dedupChans [recResultIn] recArgsIns of
        [] -> innerBody
        [_] -> Stmt resultRecv innerBody
        _more -> error "Compiler Error: Conversion from RecFun to Task Expression failed. Please report"
      finalRecv =
        Let finalResult resultRecv $
          SendData $ SSend resultOut $ Left finalResult
      epilog = case dedupChans recArgsIns [recResultIn] of
        [] -> finalRecv
        chans -> throwAwayLoopArgs chans finalRecv
   in loop $
        dispatchCtrlSig contSig $
          -- here I would have loved to have the type safety again :(
          recvInits $
            sendInits $
              Stmt
                (While condRecv loopBody)
                $ dispatchCtrlSig stopSig epilog
  where
    -- NOTE(feliix42): I changed this from `filter isLeft recInitArgsIns` because:
    --   Using even a single env arc will produce use after move errors in Rust for said arc.
    --   In every iteration of the endless loop the argument will be sent, creating ownership problems after the first iteration.
    --   My solution to this was to switch the `loop` definition.
    --   I'm happy to discuss a integration-specific solution, though.
    loop c = if any isRight recInitArgsIns then c else EndlessLoop c
    -- this loop stuff may go away once we start fusing recur
    -- loop c = case filter isLeft recInitArgsIns of
    --   [] -> c
    --   _ -> EndlessLoop c
    contSig = ctrlTuple True (Right 1) -- Tuple (Right $ BoolLit True) (Right $ NumericLit 1)
    stopSig = ctrlTuple False (Right 0) --  Tuple (Right $ BoolLit False) (Right $ NumericLit 0)
    dispatchCtrlSig sig c =
      case ctrlOut of
        Just cOut ->
          Let "ctrlSig" sig $
            Stmt
              (SendData $ SSend cOut $ Left "ctrlSig")
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
        (\(o, v) c' -> Stmt (SendData $ SSend o $ Left v) c')
        c
        $ zip recArgsOuts $
          map fst initArgsRecv
    initArgsRecv = zipWith (curry (first (("init_" <>) . show))) [0 ..] recInitArgsIns
    condRecv = ReceiveData recCondIn
    resultRecv = ReceiveData recResultIn
    loopArgsRecv = zipWith (curry (first (("loop_res_" <>) . show))) [0 ..] recArgsIns
    recvLoopArgs c = foldr (\(v, r) c' -> Let v (ReceiveData r) c') c loopArgsRecv
    throwAwayLoopArgs c ex = foldr (Stmt . ReceiveData) ex c
    sendLoopArgs c =
      foldr (\(o, v) c' -> Stmt (SendData $ SSend o $ Left v) c') c $
        zip recArgsOuts $
          map fst loopArgsRecv
    dedupChans :: [Com 'Recv embExpr annot ty] -> [Com 'Recv embExpr annot ty] -> [Com 'Recv embExpr annot ty]
    dedupChans [] _ = []
    dedupChans (x : xs) ys
      | x `elem` ys = dedupChans xs ys
      | otherwise = x : dedupChans xs ys
