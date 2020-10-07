module Ohua.Backend.Operators.If where

import Ohua.Prelude

import Ohua.Backend.Lang hiding (TCExpr)

type CondInput = Com 'Recv
type CtrlTrueOutput = Com 'Channel
type CtrlFalseOutput = Com 'Channel

ifFun :: CondInput -> CtrlTrueOutput -> CtrlFalseOutput -> TaskExpr
ifFun condInput ctrlTrue ctrlFalse = 
    Let "branchSelection" (ReceiveData condInput) $
    Cond 
        (Var "branchSelection")
        (
            Let "ctrlTrue" (Tuple (Right $ BoolLit True) (Right $ NumericLit 1)) $
            Let "ctrlFalse" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) $
            Stmt 
                (SendData $ SSend ctrlTrue "ctrlTrue")
                (SendData $ SSend ctrlFalse "ctrlFalse")
        )
        (
            Let "ctrlTrue" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) $
            Let "ctrlFalse" (Tuple (Right $ BoolLit True) (Right $ NumericLit 1)) $
            Stmt 
                (SendData $ SSend ctrlTrue "ctrlTrue")
                (SendData $ SSend ctrlFalse "ctrlFalse")
        )

type TrueBranchInput = Com 'Recv
type FalseBranchInput = Com 'Recv
type ResultOutput = Com 'Channel

select :: CondInput -> TrueBranchInput -> FalseBranchInput -> ResultOutput -> TaskExpr
select condInput trueBranchInput falseBranchInput resultOut = 
    Let "branchSelection" (ReceiveData condInput) $
        Cond 
            (Var "branchSelection")
            (
                Let "result" (ReceiveData trueBranchInput) $
                    SendData $ SSend resultOut "result"
            )
            (
                Let "result" (ReceiveData falseBranchInput) $
                    SendData $ SSend resultOut "result"
            )