module Ohua.Backend.Operators.If where

import Ohua.Commons.Prelude

import Ohua.Backend.Lang hiding (TCExpr)
import Ohua.Backend.Operators.Common (ctrlTuple)

import qualified Data.List.NonEmpty as NE (reverse)


type CondInput = Com 'Recv
type CtrlTrueOutput = Com 'Channel
type CtrlFalseOutput = Com 'Channel

ifFun :: CondInput embExpr ty -> NonEmpty (CtrlTrueOutput embExpr ty, CtrlFalseOutput embExpr ty) -> TaskExpr embExpr ty
ifFun condInput ctrlOuts =
    Let "branchSelection" (ReceiveData condInput) $
    Cond
        (Var "branchSelection")
        (dataOut (ctrlTuple True (Right 1)) (ctrlTuple True (Right 0)))
        (dataOut (ctrlTuple True (Right 0)) (ctrlTuple True (Right 1)))
    where
      dataOut ctrlTrueOut ctrlFalseOut =
        let
          ((lastChanOutTrue, lastChanOutFalse) :| ctrls) = NE.reverse ctrlOuts
          ctrlOuts' = reverse ctrls
        in
          Let "ctrlTrue" ctrlTrueOut $
          Let "ctrlFalse" ctrlFalseOut $
          foldr
           (\(chanOutTrue,chanOutFalse) cont ->
              Stmt
                (SendData $ SSend chanOutTrue $ Left "ctrlTrue")
                (Stmt
                  (SendData $ SSend chanOutFalse $ Left "ctrlFalse")
                  cont)
           )
           (Stmt
             (SendData $ SSend lastChanOutTrue $ Left "ctrlTrue")
             (SendData $ SSend lastChanOutFalse $ Left "ctrlFalse"))
           ctrlOuts'


type TrueBranchInput = Com 'Recv
type FalseBranchInput = Com 'Recv
type ResultOutput = Com 'Channel

select :: CondInput embExpr ty -> TrueBranchInput embExpr ty -> FalseBranchInput embExpr ty -> ResultOutput embExpr ty -> TaskExpr embExpr ty
select condInput trueBranchInput falseBranchInput resultOut =
    Let "branchSelection" (ReceiveData condInput) $
        Cond
            (Var "branchSelection")
            (
                Let "result" (ReceiveData trueBranchInput) $
                    SendData $ SSend resultOut $ Left "result"
            )
            (
                Let "result" (ReceiveData falseBranchInput) $
                    SendData $ SSend resultOut $ Left "result"
            )
