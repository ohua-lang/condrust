module Ohua.Backend.Operators.SMap where

import Ohua.Prelude

import Ohua.Backend.Lang hiding (TCExpr)


type Input = Binding
type DataOut = Binding
type CtrlOut = Binding
type CollectOut = Binding

smapFun :: Input -> DataOut -> CtrlOut -> CollectOut -> TaskExpr
smapFun input dataOut ctrlOut collectOut = 
    LetT "data" (ReceiveT 0 input) $
    LetT "hasSize" (HasSize "data") $
        Cond (VarT "hasSize")
            -- known size
            (
                LetT "size" (Size "data") $
                Stmt (SendT collectOut "size") $
                LetT "ctrl" (Tuple (Right $ BoolLit True) (Left "size")) $
                Stmt (SendT ctrlOut "ctrl") $ LitT UnitLit
            )
            -- unknown size --> generator-style
            (
                LetT "size" (LitT $ NumericLit 0) $
                Stmt
                    (LoopT "d" "data" $
                        Stmt (SendT dataOut "d") $
                        LetT "ctrl" (Tuple (Right $ BoolLit False) (Right $ NumericLit 1)) $
                        Stmt (SendT ctrlOut "ctrl") $ Increment "size") $
                Stmt (SendT collectOut "size") $
                LetT "ctrl" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) $
                Stmt (SendT ctrlOut "ctrl") $ LitT UnitLit
            )

type SizeInput = Binding
type DataInput = Binding
type StateInput = Binding
type CollectFun = FunRef
type CollectedOutput = Binding

collect :: SizeInput 
        -> DataInput 
        -> StateInput
        -> CollectFun
        -> CollectedOutput 
        -> TaskExpr
collect sizeInput dataInput stateInput (FunRef collectFun _) collectedOutput = 
    LetT "num" (ReceiveT 0 sizeInput) $
    LetT "collection" (ReceiveT 0 stateInput) $
    LetT "receives" (Generate "num" UnitLit) $
    Stmt 
        (
            LoopT "_receive" "receives" $
                LetT "data" (ReceiveT 0 dataInput) $
                    ApplyT $ Stateful "collection"  collectFun [VarT "data"]
        ) $
        SendT collectedOutput "collection"