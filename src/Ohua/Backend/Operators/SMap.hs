module Ohua.Backend.Operators.SMap where

import Ohua.Prelude

import Ohua.Backend.Lang


type Input = Binding
type DataOut = Binding
type CtrlOut = Binding
type CollectOut = Binding

smapFun :: Input -> DataOut -> CtrlOut -> CollectOut -> TaskExpr
smapFun input dataOut ctrlOut collectOut = 
    Let "data" (Receive 0 input) $
    Let "hasSize" (HasSize "data") $
        Cond (Var "hasSize")
            -- known size
            (
                Let "size" (Size "data") $
                Stmt (Send collectOut "size") $
                Let "ctrl" (Tuple (Right $ BoolLit True) (Left "size")) $
                Stmt (Send ctrlOut "ctrl") $ Lit UnitLit
            )
            -- unknown size --> generator-style
            (
                Let "size" (Lit $ NumericLit 0) $
                Stmt
                    (Loop "d" "data" $
                        Stmt (Send dataOut "d") $
                        Let "ctrl" (Tuple (Right $ BoolLit False) (Right $ NumericLit 1)) $
                        Stmt (Send ctrlOut "ctrl") $ Increment "size") $
                Stmt (Send collectOut "size") $
                Let "ctrl" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) $
                Stmt (Send ctrlOut "ctrl") $ Lit UnitLit
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
    Let "num" (Receive 0 sizeInput) $
    Let "collection" (Receive 0 stateInput) $
    Let "receives" (Generate "num" UnitLit) $
    Stmt 
        (
            Loop "_receive" "receives" $
                Let "data" (Receive 0 dataInput) $
                    Apply $ Stateful "collection"  collectFun [Var "data"]
        ) $
        Send collectedOutput "collection"