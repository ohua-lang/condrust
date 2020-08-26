module Ohua.Backend.Operators.If where

import Ohua.Prelude

import Ohua.Backend.Lang hiding (TCExpr)

type CondInput = Binding
type CtrlTrueOutput = Binding
type CtrlFalseOutput = Binding

ifFun :: CondInput -> CtrlTrueOutput -> CtrlFalseOutput -> TaskExpr
ifFun condInput ctrlTrue ctrlFalse = 
    LetT "branchSelection" (ReceiveT 0 condInput) $
    Cond 
        (VarT "branchSelection")
        (
            LetT "ctrlTrue" (Tuple (Right $ BoolLit True) (Right $ NumericLit 1)) $
            LetT "ctrlFalse" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) $
            Stmt 
                (SendT ctrlTrue "ctrlTrue")
                (SendT ctrlFalse "ctrlFalse")
        )
        (
            LetT "ctrlTrue" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) $
            LetT "ctrlFalse" (Tuple (Right $ BoolLit True) (Right $ NumericLit 1)) $
            Stmt 
                (SendT ctrlTrue "ctrlTrue")
                (SendT ctrlFalse "ctrlFalse")
        )

type TrueBranchInput = Binding
type FalseBranchInput = Binding
type ResultOutput = Binding

select :: CondInput -> TrueBranchInput -> FalseBranchInput -> ResultOutput -> TaskExpr
select condInput trueBranchInput falseBranchInput resultOut = 
    LetT "branchSelection" (ReceiveT 0 condInput) $
        Cond 
            (VarT "branchSelection")
            (
                LetT "result" (ReceiveT 0 trueBranchInput) $
                    SendT resultOut "result"
            )
            (
                LetT "result" (ReceiveT 0 falseBranchInput) $
                    SendT resultOut "result"
            )