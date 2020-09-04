module Ohua.Backend.Operators.State where

import Ohua.Prelude

import Ohua.Backend.Lang


type DataSizeInput = Binding
type DataInputs = [Binding]
type StateInput = Binding
type FoldFun = FunRef
type StateOutput = Binding

-- | Currently, this is implemented as a fold over the state.
runSTCLang :: DataSizeInput 
        -> StateInput
        -> FoldFun
        -> DataInputs 
        -> StateOutput 
        -> TaskExpr
runSTCLang sizeInput stateInput (FunRef foldFun _) dataInputs stateOutput = 
    Let "num" (Receive 0 sizeInput) $
    Let "state" (Receive 0 stateInput) $
    Let "receives" (Generate "num" UnitLit) $
    Stmt 
        (
            ForEach "_receive" "receives" $
                dataReceiveCode $
                    Apply $ Stateful "state" foldFun [Var "data"]
        ) $
        Send stateOutput "state"
    where
        dataReceiveCode cont = 
            foldr (\(i,input) c -> Let ("data" <> show i) (Receive 0 input) c) cont $ zip [1..] dataInputs
        dataVars = ["data" <> show i | i <- [1..(length dataInputs)]]
