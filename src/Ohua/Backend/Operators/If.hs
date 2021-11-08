module Ohua.Backend.Operators.If where

import Ohua.Prelude

import Ohua.Backend.Lang hiding (TCExpr)

type CondInput = Com 'Recv
type CtrlTrueOutput = Com 'Channel
type CtrlFalseOutput = Com 'Channel

ifFun :: CondInput ty -> CtrlTrueOutput ty -> CtrlFalseOutput ty -> TaskExpr ty
ifFun condInput ctrlTrue ctrlFalse = 
    Let "branchSelection" (ReceiveData condInput) $
    Cond 
        (Var "branchSelection")
        (
            Let "ctrlTrue" (Tuple (Right $ BoolLit True) (Right $ NumericLit 1)) $
            Let "ctrlFalse" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) $
            Stmt 
                (SendData $ SSend ctrlTrue $ Left "ctrlTrue")
                (SendData $ SSend ctrlFalse $ Left "ctrlFalse")
        )
        (
            Let "ctrlTrue" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) $
            Let "ctrlFalse" (Tuple (Right $ BoolLit True) (Right $ NumericLit 1)) $
            Stmt 
                (SendData $ SSend ctrlTrue $ Left "ctrlTrue")
                (SendData $ SSend ctrlFalse $ Left "ctrlFalse")
        )

type TrueBranchInput = Com 'Recv
type FalseBranchInput = Com 'Recv
type ResultOutput = Com 'Channel

select :: CondInput ty -> TrueBranchInput ty -> FalseBranchInput ty -> ResultOutput ty -> TaskExpr ty
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
