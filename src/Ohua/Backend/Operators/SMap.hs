module Ohua.Backend.Operators.SMap where

import Ohua.Prelude

import Ohua.Backend.Lang


type Input = Com 'Recv
type DataOut = Com 'Channel
type CtrlOut = Com 'Channel
type CollectOut = Com 'Channel

smapFun :: Input ty -> DataOut ty -> CtrlOut ty -> CollectOut ty -> TaskExpr ty
smapFun input dataOut ctrlOut collectOut = 
    Let "data" (ReceiveData input) $
    Let "hasSize" (HasSize "data") $
        Cond (Var "hasSize")
            -- known size
            (
                Let "size" (Size "data") $
                Stmt (SendData $ SSend collectOut "size") $
                Let "ctrl" (Tuple (Right $ BoolLit True) (Left "size")) $
                Stmt (SendData $ SSend ctrlOut "ctrl") $ Lit UnitLit
            )
            -- unknown size --> generator-style
            (
                Let "size" (Lit $ NumericLit 0) $
                Stmt
                    (ForEach "d" "data" $
                        Stmt (SendData $ SSend dataOut "d") $
                        Let "ctrl" (Tuple (Right $ BoolLit False) (Right $ NumericLit 1)) $
                        Stmt (SendData $ SSend ctrlOut "ctrl") $ Increment "size") $
                Stmt (SendData $ SSend collectOut "size") $
                Let "ctrl" (Tuple (Right $ BoolLit True) (Right $ NumericLit 0)) $
                Stmt (SendData $ SSend ctrlOut "ctrl") $ Lit UnitLit
            )

type SizeInput = Com 'Recv
type DataInput = Com 'Recv
type CollectedOutput = Com 'Channel

collect :: SizeInput ty
        -> DataInput ty
        -> CollectedOutput ty
        -> TaskExpr ty
collect sizeInput dataInput collectedOutput = 
    Let "num" (ReceiveData sizeInput) $
    Let "collection" (ListOp Create) $
    Let "receives" (Generate "num" UnitLit) $
    Stmt 
        (
            ForEach "_receive" "receives" $
                Let "data" (ReceiveData dataInput) $
                    ListOp $ Append "collection" $ Var "data"
        ) $
        SendData $ SSend collectedOutput "collection"