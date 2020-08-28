module Ohua.Backend.Operators.If where

import Ohua.Prelude

import Ohua.Backend.Lang hiding (TCExpr)

type CondInput = Binding
type CtrlTrueOutput = Binding
type CtrlFalseOutput = Binding

ifFun :: CondInput -> CtrlTrueOutput -> CtrlFalseOutput -> TaskExpr
ifFun condInput ctrlTrue ctrlFalse = 
    Let "branchSelection" (Receive 0 condInput) $
    Cond 
        (Var "branchSelection")
        (
            Let "ctrlTrue" (Tuple (Right $ BoolLit True) (Right $ NumericLit 1)) $
            Let "ctrlFalse" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) $
            Stmt 
                (Send ctrlTrue "ctrlTrue")
                (Send ctrlFalse "ctrlFalse")
        )
        (
            Let "ctrlTrue" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) $
            Let "ctrlFalse" (Tuple (Right $ BoolLit True) (Right $ NumericLit 1)) $
            Stmt 
                (Send ctrlTrue "ctrlTrue")
                (Send ctrlFalse "ctrlFalse")
        )

type TrueBranchInput = Binding
type FalseBranchInput = Binding
type ResultOutput = Binding

select :: CondInput -> TrueBranchInput -> FalseBranchInput -> ResultOutput -> TaskExpr
select condInput trueBranchInput falseBranchInput resultOut = 
    Let "branchSelection" (Receive 0 condInput) $
        Cond 
            (Var "branchSelection")
            (
                Let "result" (Receive 0 trueBranchInput) $
                    Send resultOut "result"
            )
            (
                Let "result" (Receive 0 falseBranchInput) $
                    Send resultOut "result"
            )